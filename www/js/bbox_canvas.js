// bbox_canvas.js
//
// Handles drag-to-draw bounding boxes over the blank-form image in the
// template designer, redraws saved field boxes with labels, and now
// supports selecting an existing box and resizing it via corner handles.
//
// Communication contract with the R/Shiny side:
//   JS -> Shiny:  Shiny.setInputValue(newBoxInputId, {x,y,w,h}, {priority:'event'})
//                 Shiny.setInputValue(resizeBoxInputId, {id,x,y,w,h}, {priority:'event'})
//                 x,y,w,h are normalized 0-1 fractions of the image's
//                 rendered size (resolution independent).
//   Shiny -> JS:  Shiny.addCustomMessageHandler('formExtractR_render_boxes', ...)
//                 payload: { canvasId, boxes: [{id,x,y,w,h,label}] }
//
// Namespaced under window.formExtractR so multiple module instances (if
// ever needed) don't collide, and so this file has one clear entry point.

window.formExtractR = window.formExtractR || {};

(function (ns) {

  const HANDLE_SIZE = 9;      // px, on-screen size of each resize handle
  const HANDLE_HIT  = 12;     // px, click-tolerance radius for grabbing a handle
  const MIN_BOX     = 12;     // px, minimum box width/height while resizing

  // Per-canvas state, keyed by canvasId.
  const canvasState = {};     // canvasId -> boxes array (last pushed from server)
  const selectedId  = {};     // canvasId -> field_id of currently selected box, or null

  const HANDLES = ["nw", "ne", "sw", "se"];

  ns.initCanvas = function (imgId, canvasId, newBoxInputId, resizeBoxInputId) {
    const img = document.getElementById(imgId);
    const canvas = document.getElementById(canvasId);
    if (!img || !canvas) return;

    const syncSize = function () {
      canvas.width = img.clientWidth;
      canvas.height = img.clientHeight;
      canvas.style.width = img.clientWidth + "px";
      canvas.style.height = img.clientHeight + "px";
      redraw(canvasId);
    };
    syncSize();
    window.addEventListener("resize", syncSize);

    // drag state: mode is one of null | "draw" | "resize"
    let mode = null;
    let startX = 0, startY = 0;
    let activeHandle = null;   // e.g. "se"
    let activeBox = null;      // reference to the box object being resized (px, in canvasState units but tracked live in px below)
    let resizeOrigin = null;   // {x,y,w,h} in px at drag start, opposite-corner anchored

    const getPos = function (evt) {
      const rect = canvas.getBoundingClientRect();
      return {
        x: evt.clientX - rect.left,
        y: evt.clientY - rect.top
      };
    };

    // Find px-space rect for a box (normalized -> canvas px)
    const boxPx = function (b) {
      return {
        x: b.x * canvas.width,
        y: b.y * canvas.height,
        w: b.w * canvas.width,
        h: b.h * canvas.height
      };
    };

    const handlePositions = function (rectPx) {
      return {
        nw: { x: rectPx.x,            y: rectPx.y },
        ne: { x: rectPx.x + rectPx.w, y: rectPx.y },
        sw: { x: rectPx.x,            y: rectPx.y + rectPx.h },
        se: { x: rectPx.x + rectPx.w, y: rectPx.y + rectPx.h }
      };
    };

    const findHandleAt = function (pos) {
      const sel = getSelectedBox(canvasId);
      if (!sel) return null;
      const hp = handlePositions(boxPx(sel));
      for (const h of HANDLES) {
        const dx = pos.x - hp[h].x, dy = pos.y - hp[h].y;
        if (Math.sqrt(dx * dx + dy * dy) <= HANDLE_HIT) return h;
      }
      return null;
    };

    const findBoxAt = function (pos) {
      const boxes = canvasState[canvasId] || [];
      // topmost (last-drawn) first
      for (let i = boxes.length - 1; i >= 0; i--) {
        const r = boxPx(boxes[i]);
        if (pos.x >= r.x && pos.x <= r.x + r.w && pos.y >= r.y && pos.y <= r.y + r.h) {
          return boxes[i];
        }
      }
      return null;
    };

    canvas.addEventListener("mousedown", function (evt) {
      const pos = getPos(evt);

      const handle = findHandleAt(pos);
      if (handle) {
        mode = "resize";
        activeHandle = handle;
        activeBox = getSelectedBox(canvasId);
        resizeOrigin = boxPx(activeBox);
        return;
      }

      const hit = findBoxAt(pos);
      if (hit) {
        // Select this box (shows handles); does not start a drag.
        selectedId[canvasId] = hit.id;
        mode = null;
        redraw(canvasId);
        return;
      }

      // Empty area: deselect, start drawing a new box.
      selectedId[canvasId] = null;
      mode = "draw";
      startX = pos.x;
      startY = pos.y;
      redraw(canvasId);
    });

    canvas.addEventListener("mousemove", function (evt) {
      if (mode === "draw") {
        const pos = getPos(evt);
        redraw(canvasId);
        drawRect(canvasId, startX, startY, pos.x - startX, pos.y - startY, "#2b8aef", null, false);
      } else if (mode === "resize") {
        const pos = getPos(evt);
        const r = computeResizedRect(resizeOrigin, activeHandle, pos, canvas.width, canvas.height);
        // live-update the in-memory box (px) for preview redraw
        activeBox._livePx = r;
        redraw(canvasId);
      } else {
        // hover feedback: show resize cursor over a handle of the selected box
        const pos = getPos(evt);
        canvas.style.cursor = findHandleAt(pos) ? cursorForHandle(findHandleAt(pos)) : "crosshair";
      }
    });

    canvas.addEventListener("mouseup", function (evt) {
      const pos = getPos(evt);

      if (mode === "draw") {
        mode = null;
        const x = Math.min(startX, pos.x);
        const y = Math.min(startY, pos.y);
        const w = Math.abs(pos.x - startX);
        const h = Math.abs(pos.y - startY);
        redraw(canvasId);
        if (w < 6 || h < 6) return;
        Shiny.setInputValue(newBoxInputId, {
          x: x / canvas.width, y: y / canvas.height,
          w: w / canvas.width, h: h / canvas.height,
          nonce: Date.now()
        }, { priority: "event" });
        return;
      }

      if (mode === "resize") {
        mode = null;
        const r = activeBox._livePx || boxPx(activeBox);
        delete activeBox._livePx;

        if (r.w < MIN_BOX || r.h < MIN_BOX) {
          // Too small — snap back to original, ignore this resize.
          redraw(canvasId);
        } else {
          Shiny.setInputValue(resizeBoxInputId, {
            id: activeBox.id,
            x: r.x / canvas.width, y: r.y / canvas.height,
            w: r.w / canvas.width, h: r.h / canvas.height,
            nonce: Date.now()
          }, { priority: "event" });
        }
        activeHandle = null;
        activeBox = null;
        resizeOrigin = null;
      }
    });

    // Cancel an in-progress drag if the mouse leaves the canvas.
    canvas.addEventListener("mouseleave", function () {
      if (mode === "resize" && activeBox) delete activeBox._livePx;
      mode = null;
      activeHandle = null;
      activeBox = null;
      resizeOrigin = null;
      redraw(canvasId);
    });

    Shiny.setInputValue(canvasId + "_ready", Date.now(), { priority: "event" });
  };

  function getSelectedBox(canvasId) {
    const id = selectedId[canvasId];
    if (!id) return null;
    const boxes = canvasState[canvasId] || [];
    return boxes.find(function (b) { return b.id === id; }) || null;
  }

  function cursorForHandle(h) {
    return (h === "nw" || h === "se") ? "nwse-resize" : "nesw-resize";
  }

  // Given the box's original px rect, which handle is being dragged, and
  // the current mouse px position, compute the new rect. The corner
  // opposite the dragged handle stays fixed.
  function computeResizedRect(orig, handle, pos, canvasW, canvasH) {
    let left   = orig.x, top    = orig.y;
    let right  = orig.x + orig.w, bottom = orig.y + orig.h;

    const clampedX = Math.max(0, Math.min(canvasW, pos.x));
    const clampedY = Math.max(0, Math.min(canvasH, pos.y));

    if (handle === "nw") { left = clampedX; top = clampedY; }
    if (handle === "ne") { right = clampedX; top = clampedY; }
    if (handle === "sw") { left = clampedX; bottom = clampedY; }
    if (handle === "se") { right = clampedX; bottom = clampedY; }

    return {
      x: Math.min(left, right),
      y: Math.min(top, bottom),
      w: Math.abs(right - left),
      h: Math.abs(bottom - top)
    };
  }

  function redraw(canvasId) {
    const canvas = document.getElementById(canvasId);
    if (!canvas) return;
    const ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    const boxes = (canvasState[canvasId] || []);
    const selId = selectedId[canvasId];

    boxes.forEach(function (b) {
      const isSelected = b.id === selId;
      const r = b._livePx || {
        x: b.x * canvas.width, y: b.y * canvas.height,
        w: b.w * canvas.width, h: b.h * canvas.height
      };
      // Every box drawn in the Template Designer is a field now — the
      // reference-point (alignment anchor) kind that used to render
      // green here was removed along with the rest of the alignment
      // pipeline (extraction reads each whole submission in one call
      // instead of cropping via a geometric transform).
      drawRect(canvasId, r.x, r.y, r.w, r.h, "#e0770f", b.label, isSelected);
    });
  }

  function drawRect(canvasId, x, y, w, h, color, label, showHandles) {
    const canvas = document.getElementById(canvasId);
    const ctx = canvas.getContext("2d");

    ctx.strokeStyle = color;
    ctx.lineWidth = showHandles ? 2.5 : 2;
    ctx.strokeRect(x, y, w, h);

    if (label) {
      ctx.font = "12px sans-serif";
      const textWidth = ctx.measureText(label).width;
      ctx.fillStyle = color;
      ctx.fillRect(x, y - 16, textWidth + 8, 16);
      ctx.fillStyle = "#ffffff";
      ctx.fillText(label, x + 4, y - 4);
    }

    if (showHandles) {
      const pts = [
        { x: x,     y: y },
        { x: x + w, y: y },
        { x: x,     y: y + h },
        { x: x + w, y: y + h }
      ];
      ctx.fillStyle = "#ffffff";
      ctx.strokeStyle = "#e0770f";
      ctx.lineWidth = 1.5;
      pts.forEach(function (p) {
        ctx.beginPath();
        ctx.rect(p.x - HANDLE_SIZE / 2, p.y - HANDLE_SIZE / 2, HANDLE_SIZE, HANDLE_SIZE);
        ctx.fill();
        ctx.stroke();
      });
    }
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("formExtractR_render_boxes", function (msg) {
      canvasState[msg.canvasId] = msg.boxes || [];
      // If the previously-selected field no longer exists, clear selection.
      const sel = selectedId[msg.canvasId];
      if (sel && !(msg.boxes || []).some(function (b) { return b.id === sel; })) {
        selectedId[msg.canvasId] = null;
      }
      redraw(msg.canvasId);
    });
  });

})(window.formExtractR);
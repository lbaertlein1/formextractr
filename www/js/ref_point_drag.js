// ref_point_drag.js
//
// Manual review modal, "Reference Points" view: lets the user drag a
// reference point's marker to correct its matched location for THIS
// submission only — never touches the template or any other
// submission. Sends the corrected normalized (0-1) CENTER position to
// Shiny on drop.
//
// Markers render as a box (sized to the reference point's actual
// annotated dimensions, set inline per-marker in R), not a fixed-size
// dot — so while dragging, the box's CENTER needs to track the cursor
// (not its top-left corner), or the box would visibly jump to have its
// corner rather than its middle under the mouse the instant you grab
// it. The underlying data being dragged is still just a center point;
// only the on-screen rendering is box-shaped.
//
// Deliberately much simpler than bbox_canvas.js — no draw-new-box, no
// resize handles, just "grab a marker, move it, let go." Event
// delegation on `document` for the same reason as review_highlight.js:
// the modal's DOM is injected fresh each time, so a listener bound
// directly to markers that don't exist yet at page-load time would
// never fire.
(function () {
  let dragging = null; // { markerEl, wrapEl, refId, inputId, wPct, hPct }

  document.addEventListener("mousedown", function (e) {
    const marker = e.target.closest(".fe-refpoint-marker");
    if (!marker) return;
    const wrap = marker.closest(".fe-overlay-wrap");
    if (!wrap) return;

    dragging = {
      markerEl: marker,
      wrapEl: wrap,
      refId: marker.getAttribute("data-ref-id"),
      inputId: marker.getAttribute("data-drag-input"),
      // Box size (set inline by R as a % string, e.g. "8.780%") stays
      // fixed for the whole drag — only position changes.
      wPct: parseFloat(marker.style.width) || 0,
      hPct: parseFloat(marker.style.height) || 0
    };
    marker.classList.add("fe-refpoint-dragging");
    e.preventDefault();
  });

  document.addEventListener("mousemove", function (e) {
    if (!dragging) return;
    const rect = dragging.wrapEl.getBoundingClientRect();
    let px = (e.clientX - rect.left) / rect.width;
    let py = (e.clientY - rect.top) / rect.height;
    px = Math.max(0, Math.min(1, px));
    py = Math.max(0, Math.min(1, py));

    // Position the box so its CENTER (not its corner) tracks the cursor.
    const leftPct = px * 100 - dragging.wPct / 2;
    const topPct = py * 100 - dragging.hPct / 2;
    dragging.markerEl.style.left = leftPct + "%";
    dragging.markerEl.style.top = topPct + "%";

    // What actually gets sent to Shiny is the CENTER fraction, not the
    // box's corner — this is still fundamentally a point being corrected.
    dragging.markerEl.dataset.pendingX = px;
    dragging.markerEl.dataset.pendingY = py;
  });

  document.addEventListener("mouseup", function () {
    if (!dragging) return;
    const x = parseFloat(dragging.markerEl.dataset.pendingX);
    const y = parseFloat(dragging.markerEl.dataset.pendingY);
    dragging.markerEl.classList.remove("fe-refpoint-dragging");

    if (!isNaN(x) && !isNaN(y) && dragging.inputId) {
      Shiny.setInputValue(
        dragging.inputId,
        { ref_id: dragging.refId, x: x, y: y, nonce: Date.now() },
        { priority: "event" }
      );
    }
    dragging = null;
  });
})();

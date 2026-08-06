// review_highlight.js
//
// Manual review modal: when a value's text input is focused, highlight
// its matching overlay box on the form image in red, so it's obvious
// exactly which region of the form is being corrected.
//
// The link between an input and its box is a shared data-field-idx
// attribute (set in mod_extraction.R's review_modal()) — no Shiny
// round-trip needed, this is purely a client-side visual affordance.
//
// Event delegation on `document` (rather than binding to the modal's
// contents directly) because the modal is injected into the DOM after
// this script loads, and re-injected fresh every time a row is clicked.
document.addEventListener("focusin", function (e) {
  var fieldWrap = e.target.closest(".fe-review-fields-pane [data-field-idx]");

  document.querySelectorAll(".fe-overlay-box").forEach(function (box) {
    box.classList.remove("fe-overlay-box-active");
  });

  if (fieldWrap) {
    var idx = fieldWrap.getAttribute("data-field-idx");
    var box = document.querySelector('.fe-overlay-box[data-field-idx="' + idx + '"]');
    if (box) {
      box.classList.add("fe-overlay-box-active");
      // Keep the highlighted box in view if the image pane is tall
      // enough to need scrolling.
      box.scrollIntoView({ block: "nearest", behavior: "smooth" });
    }
  }
});

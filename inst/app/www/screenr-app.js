// ScreenR app: a light that follows the cursor over the glass panels, as in
// the ScreenR website
(function () {
  var still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var mouse = window.matchMedia("(pointer: fine)").matches;
  if (still || !mouse) return;
  var glass = ".navbar, .card, .bslib-value-box, .sr-hero, " +
    ".bslib-sidebar-layout > .sidebar";
  var event = null;
  var lit = null;
  var paint = function () {
    var e = event;
    event = null;
    var panel = e.target && e.target.closest ? e.target.closest(glass) : null;
    if (lit && lit !== panel) lit.classList.remove("sr-lit");
    if (panel) {
      var box = panel.getBoundingClientRect();
      panel.classList.add("sr-glow", "sr-lit");
      panel.style.setProperty("--sr-px", e.clientX - box.left + "px");
      panel.style.setProperty("--sr-py", e.clientY - box.top + "px");
    }
    lit = panel;
  };
  document.addEventListener("pointermove", function (e) {
    if (!event) window.requestAnimationFrame(paint);
    event = e;
  }, { passive: true });
  document.documentElement.addEventListener("mouseleave", function () {
    if (lit) lit.classList.remove("sr-lit");
    lit = null;
  });
})();

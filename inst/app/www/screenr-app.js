// ScreenR app: a light that follows the cursor over the glass panels, as in
// the ScreenR website
(function () {
  // The icons are decoration: the label of the tab or the button says what
  // they do, and an aria-label on an element without a role is not valid
  var plain = function () {
    document.querySelectorAll("i[aria-label][role='presentation']").forEach(
      function (icon) {
        icon.removeAttribute("aria-label");
        icon.setAttribute("aria-hidden", "true");
      });
    // The list of the tabs may only hold tabs: Tutorial and the appearance
    // switch go to a list of their own, on the right of the bar
    var tabs = document.querySelector(".navbar ul[role='tablist']");
    if (tabs && !tabs.dataset.srSplit) {
      var others = tabs.querySelectorAll(":scope > li:not(:has(> a[role='tab']))");
      if (others.length) {
        var side = document.createElement("ul");
        side.className = "nav navbar-nav nav-underline sr-side";
        others.forEach(function (item) {
          if (!item.classList.contains("bslib-nav-spacer")) side.appendChild(item);
          else item.remove();
        });
        tabs.after(side);
      }
      tabs.dataset.srSplit = "1";
    }
    // The content of the page is the main landmark
    var content = document.querySelector(".tab-content");
    if (content && !content.hasAttribute("role")) {
      content.setAttribute("role", "main");
    }
  };
  document.addEventListener("DOMContentLoaded", plain);
  new MutationObserver(plain).observe(document.documentElement,
    { childList: true, subtree: true });

  var still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var mouse = window.matchMedia("(pointer: fine)").matches;
  if (still || !mouse) return;
  var glass = ".navbar, .bslib-sidebar-layout > .sidebar";
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

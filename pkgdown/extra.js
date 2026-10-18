// ScreenR pkgdown theme: a stronger glass navbar once the page is scrolled.
(function () {
  var update = function () {
    document.body.classList.toggle("sr-scrolled", window.scrollY > 8);
  };
  document.addEventListener("DOMContentLoaded", update);
  window.addEventListener("scroll", update, { passive: true });
})();

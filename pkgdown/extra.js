// ScreenR pkgdown theme: interactions of the glass design.
// Every feature only adds to the page: without JavaScript the site works as
// plain pkgdown.
(function () {
  var still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var mouse = window.matchMedia("(hover: hover) and (pointer: fine)").matches;
  var root = document.documentElement;

  // Path of the site root, from the link of the navbar brand
  var siteRoot = function () {
    var brand = document.querySelector(".navbar-brand");
    return brand ? brand.getAttribute("href").replace(/index\.html$/, "") : "";
  };

  var tutorials = [
    ["Tutorial_1_Creating_the_object", "Getting your data into ScreenR"],
    ["Tutorial_2_Quality_control", "Quality control"],
    ["Tutorial_3_Finding_hits", "Finding and validating hits"],
    ["Tutorial_4_CRISPR_screens", "CRISPR-Cas9 screens"]
  ];

  // Stronger glass navbar once the page is scrolled, and reading progress
  var progress = null;
  var onScroll = function () {
    document.body.classList.toggle("sr-scrolled", window.scrollY > 8);
    if (progress) {
      var max = document.documentElement.scrollHeight - window.innerHeight;
      progress.style.transform = "scaleX(" + (max > 0 ? window.scrollY / max : 0) + ")";
    }
  };

  // Home page: buttons and highlights under the tagline
  var hero = function () {
    if (!document.querySelector(".template-home")) return;
    var tagline = document.querySelector("main .section.level1 > p");
    if (!tagline) return;
    var base = siteRoot();
    var box = document.createElement("div");
    box.className = "sr-hero";
    box.innerHTML =
      '<div class="sr-chips">' +
      '<span class="sr-chip">p-value and FDR for every gene</span>' +
      '<span class="sr-chip">shRNA and CRISPR-Cas9</span>' +
      '<span class="sr-chip">Benchmarked against MAGeCK and BAGEL2</span>' +
      "</div>" +
      '<div class="sr-actions">' +
      '<a class="sr-btn sr-btn-primary" href="' + base + 'articles/Tutorial_1_Creating_the_object.html">Get started</a>' +
      '<a class="sr-btn" href="#installation">Install</a>' +
      '<a class="sr-btn" href="' + base + 'articles/Benchmark_MAGeCK.html">Benchmark</a>' +
      "</div>";
    tagline.after(box);
  };

  // Tutorials: progress through the four tutorials and previous/next links
  var tutorialNav = function () {
    var page = window.location.pathname.split("/").pop().replace(/\.html$/, "");
    var index = tutorials.findIndex(function (t) { return t[0] === page; });
    var main = document.querySelector("main");
    if (index < 0 || !main) return;
    var link = function (i, label) {
      return '<a class="sr-btn" href="' + tutorials[i][0] + '.html"><small>' +
        label + "</small>" + (i + 1) + ". " + tutorials[i][1] + "</a>";
    };
    var steps = tutorials.map(function (t, i) {
      return '<a href="' + t[0] + '.html" class="sr-step' +
        (i === index ? " sr-step-current" : i < index ? " sr-step-done" : "") +
        '" title="' + t[1] + '">' + (i + 1) + "</a>";
    }).join('<span class="sr-step-line"></span>');
    var nav = document.createElement("nav");
    nav.className = "sr-tutorial-nav";
    nav.setAttribute("aria-label", "Tutorials");
    nav.innerHTML = '<div class="sr-steps">' + steps + "</div>" +
      '<div class="sr-prev-next">' +
      (index > 0 ? link(index - 1, "Previous") : "<span></span>") +
      (index < tutorials.length - 1 ? link(index + 1, "Next") : "<span></span>") +
      "</div>";
    main.appendChild(nav);
  };

  // Reading progress bar on articles and help pages
  var readingProgress = function () {
    if (!document.querySelector(".template-article, .template-reference-topic")) return;
    progress = document.createElement("div");
    progress.className = "sr-progress";
    document.body.appendChild(progress);
  };

  // Reference index: one card for each section (pkgdown puts the title and
  // the functions of a section in two consecutive divs)
  var referenceCards = function () {
    if (!document.querySelector(".template-reference-index")) return;
    document.querySelectorAll("main > .section.level2").forEach(function (section) {
      var next = section.nextElementSibling;
      if (section.querySelector("h2") && next && next.matches(".section.level2") &&
        !next.querySelector("h2")) {
        while (next.firstChild) section.appendChild(next.firstChild);
        next.remove();
      }
      section.classList.add("sr-card");
    });
  };

  // Code blocks: a label with the language
  var codeLabels = function () {
    document.querySelectorAll("main pre.sourceCode.r").forEach(function (pre) {
      pre.setAttribute("data-lang", "R");
    });
  };

  // Figures: click to zoom
  var zoom = function () {
    var images = document.querySelectorAll("main img.r-plt, main p > img[src*='figures/']");
    if (!images.length) return;
    var overlay = document.createElement("div");
    overlay.className = "sr-zoom";
    overlay.setAttribute("role", "dialog");
    overlay.setAttribute("aria-label", "Enlarged figure");
    overlay.innerHTML = '<img alt="">';
    document.body.appendChild(overlay);
    var big = overlay.querySelector("img");
    var close = function () { overlay.classList.remove("sr-open"); };
    images.forEach(function (img) {
      img.classList.add("sr-zoomable");
      img.setAttribute("tabindex", "0");
      var open = function () {
        big.src = img.currentSrc || img.src;
        big.alt = img.alt;
        overlay.classList.add("sr-open");
      };
      img.addEventListener("click", open);
      img.addEventListener("keydown", function (e) {
        if (e.key === "Enter") open();
      });
    });
    overlay.addEventListener("click", close);
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") close();
    });
  };

  // Search: Cmd/Ctrl + K focuses the search field
  var searchShortcut = function () {
    var input = document.getElementById("search-input");
    if (!input) return;
    var mac = /Mac|iPhone|iPad/.test(navigator.platform);
    input.setAttribute("placeholder", "Search  " + (mac ? "⌘K" : "Ctrl K"));
    document.addEventListener("keydown", function (e) {
      if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "k") {
        e.preventDefault();
        input.focus();
        input.select();
      }
    });
  };

  // Sections, figures, code and tables rise into view while scrolling
  var reveal = function () {
    if (still || !("IntersectionObserver" in window)) return;
    var items = document.querySelectorAll(
      "main h2, main p > img, main pre, main table, .sr-card");
    var observer = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        if (entry.isIntersecting) {
          entry.target.classList.add("sr-in");
          observer.unobserve(entry.target);
        }
      });
    }, { rootMargin: "0px 0px -8% 0px" });
    items.forEach(function (item) {
      // Only what is below the first screen: no flash at load
      if (item.getBoundingClientRect().top > window.innerHeight) {
        item.classList.add("sr-reveal");
        observer.observe(item);
      }
    });
  };

  // Workflow diagram of the home page: a light sweeps over it when it
  // comes into view
  var workflow = function () {
    var img = document.querySelector("main img[src*='workflow.svg']");
    if (!img || still) return;
    var wrap = document.createElement("span");
    wrap.className = "sr-sweep";
    img.parentNode.insertBefore(wrap, img);
    wrap.appendChild(img);
    if (!("IntersectionObserver" in window)) return;
    new IntersectionObserver(function (entries, observer) {
      if (entries[0].isIntersecting) {
        wrap.classList.add("sr-sweep-run");
        observer.disconnect();
      }
    }, { threshold: 0.5 }).observe(wrap);
  };

  // Liquid glass: refraction of the navbar, where the browser supports SVG
  // filters in backdrop-filter (Chromium)
  var refraction = function () {
    var chromium = navigator.userAgentData && navigator.userAgentData.brands &&
      navigator.userAgentData.brands.some(function (b) { return b.brand === "Chromium"; });
    if (!chromium || still) return;
    var svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("aria-hidden", "true");
    svg.setAttribute("class", "sr-filters");
    svg.innerHTML =
      '<filter id="sr-refract" x="0" y="0" width="100%" height="100%">' +
      '<feTurbulence type="fractalNoise" baseFrequency="0.006 0.02" numOctaves="2" seed="7" result="noise"/>' +
      '<feGaussianBlur in="noise" stdDeviation="2" result="soft"/>' +
      '<feDisplacementMap in="SourceGraphic" in2="soft" scale="22" xChannelSelector="R" yChannelSelector="G"/>' +
      "</filter>";
    document.body.appendChild(svg);
    root.classList.add("sr-refraction");
  };

  // Light that follows the cursor over the navbar, the glass of the page
  var cursor = function () {
    if (!mouse || still) return;
    var glass = ".navbar";
    var event = null;
    var lit = null;
    var paint = function () {
      var e = event;
      event = null;
      root.style.setProperty("--sr-mx", e.clientX + "px");
      root.style.setProperty("--sr-my", e.clientY + "px");
      var target = e.target.closest ? e.target : null;
      var panel = target ? target.closest(glass) : null;
      if (lit && lit !== panel) lit.classList.remove("sr-lit");
      if (panel) {
        var box = panel.getBoundingClientRect();
        panel.style.setProperty("--sr-px", e.clientX - box.left + "px");
        panel.style.setProperty("--sr-py", e.clientY - box.top + "px");
        panel.classList.add("sr-lit");
      }
      lit = panel;
    };
    document.addEventListener("pointermove", function (e) {
      if (!event) window.requestAnimationFrame(paint);
      event = e;
      document.body.classList.add("sr-pointer");
    }, { passive: true });
    root.addEventListener("mouseleave", function () {
      document.body.classList.remove("sr-pointer");
      if (lit) lit.classList.remove("sr-lit");
      lit = null;
    });
  };

  // CRISPR scissors: a small pair of scissors cuts the DNA of the logo in
  // half, on the home page once and whenever the logo is hovered or clicked
  var scissors = function () {
    var logo = document.querySelector(".page-header img.logo");
    if (!logo || still) return;
    var wrap = document.createElement("span");
    wrap.className = "sr-logo";
    wrap.setAttribute("title", "Cas9 at work");
    // The DNA of the logo alone, next to the logo in the site
    wrap.style.setProperty("--sr-dna",
      'url("' + logo.src.replace(/logo\.png$/, "dna.png") + '")');
    logo.parentNode.insertBefore(wrap, logo);
    wrap.appendChild(logo);
    var blade = function (cls, y, ring) {
      // The back of each blade crosses the pivot towards the other side
      var back = y < 20 ? 23 : 17;
      return '<g class="' + cls + '">' +
        '<path d="M30 20 L96 ' + y + ' L34 ' + back + ' Z" fill="#EEF3F2" stroke="#4C5D5A" stroke-width="1"/>' +
        '<path d="M21 ' + (ring < 20 ? 14 : 26) + ' L30 20" stroke="#C9836A" stroke-width="4" stroke-linecap="round"/>' +
        '<ellipse cx="13" cy="' + ring + '" rx="10" ry="7" fill="none" stroke="#C9836A" stroke-width="4"/></g>';
    };
    wrap.insertAdjacentHTML("beforeend",
      '<span class="sr-dna"><span class="sr-patch"></span>' +
      '<span class="sr-half sr-half-a"><span class="sr-strand"></span></span>' +
      '<span class="sr-half sr-half-b"><span class="sr-strand"></span></span>' +
      '<span class="sr-spark"></span></span>' +
      '<svg class="sr-scissors" viewBox="0 0 100 40" aria-hidden="true">' +
      blade("sr-blade-a", 15, 9) + blade("sr-blade-b", 25, 31) +
      '<circle cx="30" cy="20" r="2.4" fill="#1F5F55"/></svg>');
    var running = false;
    var cut = function () {
      if (running) return;
      running = true;
      wrap.classList.add("sr-cutting");
      // The DNA snaps into a V when the scissors reach the hinge
      setTimeout(function () { wrap.classList.add("sr-cut", "sr-split"); }, 1300);
      setTimeout(function () { wrap.classList.remove("sr-split"); }, 2500);
      setTimeout(function () {
        wrap.classList.remove("sr-cut", "sr-cutting");
        running = false;
      }, 3300);
    };
    wrap.addEventListener("mouseenter", cut);
    wrap.addEventListener("click", cut);
    if (document.querySelector(".template-home")) setTimeout(cut, 1200);
  };

  // Assistive technology: links without a name are hidden from it (the
  // anchors of the lines of code) or removed (an email that is not there)
  var names = function () {
    document.querySelectorAll("pre a[href^='#cb']").forEach(function (a) {
      a.setAttribute("aria-hidden", "true");
      a.setAttribute("tabindex", "-1");
    });
    document.querySelectorAll("a.author_email[href$='mailto:#']").forEach(function (a) {
      a.remove();
    });
  };

  document.addEventListener("DOMContentLoaded", function () {
    names();
    hero();
    scissors();
    tutorialNav();
    readingProgress();
    referenceCards();
    codeLabels();
    zoom();
    searchShortcut();
    workflow();
    reveal();
    refraction();
    cursor();
    onScroll();
  });
  window.addEventListener("scroll", onScroll, { passive: true });
})();

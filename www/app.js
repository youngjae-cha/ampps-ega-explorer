(function (root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory;
  else factory(root);
}(typeof window !== 'undefined' ? window : this, function (win) {
  'use strict';
  var doc = win.document;
  var stages = ['define', 'map', 'results', 'interpret'];
  var nextStages = {to_map: 'map', to_results: 'results', to_interpret: 'interpret'};
  var navigating = false;
  var handlersRegistered = false;
  var buildBusy = false;
  var buildLabel = null;

  function later(callback) {
    (win.requestAnimationFrame || win.setTimeout).call(win, callback);
  }

  function stageLink(stage) {
    if (stages.indexOf(stage) < 0) return null;
    return doc.querySelector('#stage a[data-value="' + stage + '"]');
  }

  function isActive(link) {
    return link.classList.contains('active') ||
      (link.parentElement && link.parentElement.classList.contains('active'));
  }

  function focusStage(link) {
    if (!isActive(link)) return;
    var target = link.getAttribute('href');
    var pane = target && target.charAt(0) === '#' ? doc.getElementById(target.slice(1)) : null;
    var heading = pane && pane.querySelector('.step-lead h2');
    if (heading) {
      heading.setAttribute('tabindex', '-1');
      heading.focus({preventScroll: true});
    }
    var reduceMotion = win.matchMedia && win.matchMedia('(prefers-reduced-motion: reduce)').matches;
    win.scrollTo({top: 0, left: 0, behavior: reduceMotion ? 'auto' : 'smooth'});
    // Plotly/DT outputs becoming visible need the final pane dimensions.
    win.dispatchEvent(new win.Event('resize'));
  }

  function navigateStage(message) {
    var stage = typeof message === 'string' ? message : message && message.stage;
    var link = stageLink(stage);
    if (!link) return false;
    // Use the same native click path as the top navigation. Calling $.fn.tab
    // directly can fail if another htmlwidget has replaced that plugin.
    navigating = true;
    try { link.click(); } finally { navigating = false; }
    later(function () {
      if (!isActive(link)) {
        if (win.bootstrap && win.bootstrap.Tab) {
          var Tab = win.bootstrap.Tab;
          if (typeof Tab.getOrCreateInstance === 'function') Tab.getOrCreateInstance(link).show();
          else new Tab(link).show();
        } else if (win.jQuery && win.jQuery.fn.tab && win.jQuery.fn.tab.Constructor) {
          // Bootstrap 3 constructor fallback; do not invoke a replaced $.fn.tab.
          new win.jQuery.fn.tab.Constructor(link).show();
        }
      }
      focusStage(link);
    });
    return true;
  }

  function setBuildBusy(message) {
    buildBusy = typeof message === 'boolean' ? message : !!(message && message.busy);
    var button = doc.getElementById('run');
    if (!button) return;
    if (buildLabel === null) buildLabel = button.textContent;
    button.textContent = buildBusy ? 'Building analysis…' : buildLabel;
    button.setAttribute('aria-busy', buildBusy ? 'true' : 'false');
    button.classList.toggle('is-building', buildBusy);
  }

  function registerHandlers() {
    if (handlersRegistered || !win.Shiny || !win.Shiny.addCustomMessageHandler) return;
    handlersRegistered = true;
    win.Shiny.addCustomMessageHandler('navigateStage', navigateStage);
    win.Shiny.addCustomMessageHandler('analysisBuildState', setBuildBusy);
    win.Shiny.addCustomMessageHandler('resetOptionalFiles', function (ids) {
      ids.forEach(function (id) {
        var el = doc.getElementById(id);
        if (!el) return;
        el.value = '';
        var container = win.jQuery(el).closest('.shiny-input-container');
        container.find('input[type=text]').val('');
        container.find('.progress').css('visibility', 'hidden');
      });
    });
  }

  function initialize() {
    // Explicit Next controls are immediate; changing model/focal/boundary
    // settings never triggers navigation. Build advances only on server success.
    doc.addEventListener('click', function (event) {
      var button = event.target.closest && event.target.closest('button');
      if (button && nextStages[button.id]) {
        event.preventDefault();
        navigateStage(nextStages[button.id]);
      }
      if (button && button.id === 'run') {
        if (buildBusy) {
          event.preventDefault();
          event.stopImmediatePropagation();
        } else setBuildBusy(true);
      }
      var link = event.target.closest && event.target.closest('#stage a[data-value]');
      if (link && !navigating) later(function () { focusStage(link); });
    }, true);
    if (win.jQuery) {
      win.jQuery(doc).on('shiny:connected.amppsNavigation', registerHandlers);
      win.jQuery(doc).on('shiny:disconnected.amppsNavigation', function () { setBuildBusy(false); });
    }
    registerHandlers();
  }
  if (doc.readyState === 'loading') doc.addEventListener('DOMContentLoaded', initialize, {once: true});
  else initialize();
  return {navigateStage: navigateStage, setBuildBusy: setBuildBusy};
}));

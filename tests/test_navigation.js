// Dependency-free unit tests of the real navigation script. Browser QA also
// exercises native Bootstrap and Shiny, which these small DOM mocks cannot.
'use strict';
const assert = require('node:assert/strict');
const createNavigation = require('../www/app.js');

function harness(options = {}) {
  const queue = [], clicks = [], scrolls = [], focused = [], messages = {};
  const listeners = {}, jqListeners = {}, links = {}, panes = {};
  const classes = () => {
    const values = new Set();
    return {contains: x => values.has(x), add: x => values.add(x),
      remove: x => values.delete(x), toggle: (x, on) => on ? values.add(x) : values.delete(x)};
  };
  function activate(stage) {
    Object.values(links).forEach(link => link.parentElement.classList.remove('active'));
    links[stage].parentElement.classList.add('active');
  }
  ['define', 'map', 'results', 'interpret'].forEach(stage => {
    const heading = {setAttribute() {}, focus: settings => focused.push({stage, settings})};
    panes['pane-' + stage] = {querySelector: selector => selector === '.step-lead h2' ? heading : null};
    links[stage] = {classList: classes(), parentElement: {classList: classes()},
      getAttribute: attr => attr === 'href' ? '#pane-' + stage : null,
      click: () => { clicks.push(stage); if (options.nativeActivates !== false) activate(stage); }};
  });
  activate('define');
  const build = {id: 'run', textContent: 'Build analysis', classList: classes(), attributes: {},
    setAttribute(name, value) { this.attributes[name] = value; }};
  const doc = {readyState: 'complete',
    addEventListener: (name, handler) => { listeners[name] = handler; },
    getElementById: id => id === 'run' ? build : panes[id],
    querySelector: selector => links[(selector.match(/data-value="([a-z]+)"/) || [])[1]] || null};
  const jq = () => ({on: (name, handler) => { jqListeners[name] = handler; }});
  jq.fn = {tab: () => { throw new Error('Replaced $.fn.tab must not be called'); }};
  const win = {document: doc, jQuery: jq,
    Shiny: {addCustomMessageHandler: (name, handler) => { messages[name] = handler; }},
    requestAnimationFrame: fn => queue.push(fn),
    scrollTo: settings => scrolls.push(settings), dispatchEvent() {}, Event: function (type) { this.type = type; },
    matchMedia: () => ({matches: !!options.reduceMotion})};
  if (options.bootstrapFallback) win.bootstrap = {Tab: {getOrCreateInstance: link => ({show: () => {
    activate(Object.keys(links).find(stage => links[stage] === link));
  }})}};
  const api = createNavigation(win);
  function flush() { while (queue.length) queue.shift()(); }
  function clickButton(id) {
    const button = id === 'run' ? build : {id};
    const event = {target: {closest: selector => selector === 'button' ? button : null},
      prevented: false, stopped: false,
      preventDefault() { this.prevented = true; }, stopImmediatePropagation() { this.stopped = true; }};
    listeners.click(event);
    return event;
  }
  return {api, links, clicks, scrolls, focused, messages, listeners, jqListeners, build, flush, clickButton};
}

const h = harness();
assert.equal(h.api.navigateStage({stage: 'map'}), true);
h.flush();
assert.deepEqual(h.clicks, ['map']);
assert.equal(h.links.map.parentElement.classList.contains('active'), true);
assert.equal(h.focused[0].stage, 'map');
assert.deepEqual(h.scrolls[0], {top: 0, left: 0, behavior: 'smooth'});
assert.equal(h.api.navigateStage({stage: 'unexpected" onclick="alert(1)'}), false);
assert.equal(h.api.navigateStage(null), false);
assert.equal(h.clicks.length, 1);
for (const [id, stage] of [['to_map', 'map'], ['to_results', 'results'], ['to_interpret', 'interpret']]) {
  h.clickButton(id); h.flush();
  assert.equal(h.clicks.at(-1), stage);
  assert.equal(h.focused.at(-1).stage, stage);
}
const beforeBuild = h.clicks.length;
assert.equal(h.clickButton('run').stopped, false);
assert.equal(h.build.textContent, 'Building analysis…');
assert.equal(h.build.attributes['aria-busy'], 'true');
assert.equal(h.clickButton('run').stopped, true); // duplicate run is suppressed
h.flush();
assert.equal(h.clicks.length, beforeBuild); // no navigation until server success
h.messages.analysisBuildState({busy: false});
assert.equal(h.build.textContent, 'Build analysis');
assert.equal(h.build.attributes['aria-busy'], 'false');
h.messages.navigateStage({stage: 'map'}); h.flush();
assert.equal(h.clicks.at(-1), 'map');
h.clickButton('run');
h.jqListeners['shiny:disconnected.amppsNavigation']();
assert.equal(h.build.attributes['aria-busy'], 'false');
assert.equal(h.clickButton('run').stopped, false);

const reduced = harness({reduceMotion: true});
reduced.api.navigateStage('map'); reduced.flush();
assert.equal(reduced.scrolls[0].behavior, 'auto');
const fallback = harness({nativeActivates: false, bootstrapFallback: true});
fallback.api.navigateStage('results'); fallback.flush();
assert.equal(fallback.links.results.parentElement.classList.contains('active'), true);
assert.equal(fallback.focused[0].stage, 'results');
assert.equal(Object.keys(h.listeners).includes('change'), false); // no settings-triggered jump
console.log('PASS: native tab activation, Next controls, success-only Build navigation, busy/reset guard, scroll/focus, reduced motion, Bootstrap fallback, invalid targets.');

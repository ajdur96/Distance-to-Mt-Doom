import { COPY, fmt, listify } from "./copy.js";

/* ---------- small helpers ---------- */
const curly = (s) => String(s).replace(/'/g, "’");
const ROMAN = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI"];
const DISPLAY = { Lothlorien: "Lothlórien" };
const KM_PER_MI = 1.609344;
const FT_PER_M = 3.28084;
const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

/* ---------- text that needs no data, shown before anything loads ---------- */
document.getElementById("title").textContent = COPY.title;
document.getElementById("subtitle").textContent = COPY.subtitle;
document.getElementById("intro").innerHTML = COPY.intro.map((p) => `<p>${p}</p>`).join("");
document.getElementById("yours-title").textContent = COPY.yoursTitle;
document.getElementById("yours-intro").textContent = COPY.yoursIntro;
document.getElementById("yours-reset").textContent = COPY.yoursReset;
document.getElementById("yours-shake").textContent = COPY.yoursShake;
document.getElementById("yours-locate").textContent = COPY.yoursLocate;
document.getElementById("where").textContent = COPY.loading;

/* ---------- data: the story's files load first; Your Journey's load later ---------- */
const getJson = (name) => fetch(`data/${name}`).then((r) => {
  if (!r.ok) throw new Error(`${name}: ${r.status}`);
  return r.json();
});
// Files only the Your Journey tab needs, fetched once and shared
const lazy = (fn) => { let p = null; return () => (p ??= fn()); };
const loadLandmarkFile = lazy(() => getJson("landmarks.json"));
const loadPlacesFile = lazy(() => getJson("places.json"));
const loadPaceFile = lazy(() => fetch("data/continent-pace.bin").then((r) => r.arrayBuffer()));

const [DATA, CONT, ROAD1, ROAD2, PHOTO_FILE] = await Promise.all(
  ["frodo.json", "continent.json", "road1.json", "road2.json", "photos.json"].map(getJson));
const milestones = DATA.milestones.map((m) => ({ ...m, display: DISPLAY[m.name] || m.name }));
const META = DATA.paceMeta;
// Continent-wide data, for the Hobbit Trail road and the Your Journey tab
const YMETA = CONT.paceMeta;
const lon0 = META.projection.lon0, lat0 = META.projection.lat0;
const bagEnd = [lon0, lat0];

// Everything the story map needs that differs between the two roads
const ROADS = {
  // Both roads' rings come from the same continent walking model as the roads themselves
  1: { origin: bagEnd, rings: ROAD1.rings, ringLines: ROAD1.ringLines,
       landmarksByRing: d3.group(ROAD1.ringLandmarks, (d) => d.nearestRing), base: DATA, glyphs: () => MAIN_GLYPHS,
       nested: true },
  2: { origin: [-124.105, 44.17], rings: ROAD2.rings, ringLines: ROAD2.ringLines,
       landmarksByRing: d3.group(ROAD2.ringLandmarks, (d) => d.nearestRing), base: null, glyphs: () => CONT_GLYPHS,
       nested: true }  // rings computed in the browser are nested areas rather than bands
};
const road = () => ROADS[activeChain];
function profileFor(chainId) {
  const P = DATA.profiles, out = [];
  for (let i = 0; i < P.distKm.length; i++) {
    if (P.chainId[i] === chainId) out.push({ d: P.distKm[i] / KM_PER_MI, e: (P.elevM[i] ?? 0) * FT_PER_M, lon: P.lon[i], lat: P.lat[i], stop: P.stop[i] });
  }
  return out;
}
function chainInfo(chainId) {
  const stops = DATA.stops.features.filter((f) => f.properties.chainId === chainId).map((f) => ({ ...f.properties, coords: f.geometry.coordinates, label: curly(f.properties.label) }));
  const profile = profileFor(chainId);
  const legs = DATA.legs.filter((l) => l.chainId === chainId);
  const ringStop = [], detourStop = [], stepIndex = [0];
  for (let k = 1; k <= milestones.length; k++) {
    ringStop[k] = stops.find((s) => s.role === "ring" && s.step === k);
    detourStop[k] = stops.find((s) => s.role === "detour" && s.step === k);
    const idx = profile.findIndex((p) => p.stop && curly(p.stop) === ringStop[k].label);
    stepIndex[k] = idx >= 0 ? idx : profile.length - 1;
  }
  return { chainId, stops, profile, legs, ringStop, detourStop, stepIndex };
}
const CHAINS = { 1: chainInfo(1), 2: chainInfo(2) };
const ROUTE_COLOR = { 1: "var(--blood)", 2: "var(--verdigris)" };

/* ---------- text that needs the data ---------- */
const PHOTOS = PHOTO_FILE.photos;
const photoFor = (road, step) => PHOTOS.find((p) => p.road === road && p.step === step);
const esc = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
const photoCreditsHtml = [1, 2].map((road) => `<h4>${COPY.roadName[road]}</h4><ol class="photo-credits">` +
  PHOTOS.filter((p) => p.road === road).map((p) => {
    const where = esc(CHAINS[road].ringStop[p.step].label);
    if (!p.author) return `<li>${where}: <span class="placeholder">credit needed</span></li>`;
    const by = p.commonsPage ? `<a href="${esc(p.commonsPage)}">${esc(p.author)}</a>` : esc(p.author);
    const lic = p.licenseUrl ? `<a href="${esc(p.licenseUrl)}">${esc(p.license)}</a>` : esc(p.license);
    return `<li>${where}: ${by}, ${lic}</li>`;
  }).join("") + `</ol>`).join("");
const frictionTableHtml = `<table class="friction"><thead><tr><th>Land cover</th><th>Walking time ×</th></tr></thead><tbody>` +
  COPY.frictionTable.map(([c, m]) => `<tr><td>${c}</td><td>${m}</td></tr>`).join("") + `</tbody></table>`;
document.getElementById("method-content").innerHTML = `<h2>${COPY.notesTitle}</h2>` +
  COPY.methodSections.map((sec) => `<h3>${sec.title}</h3>` +
    sec.paragraphs.map((p) => (p === "[table]" ? frictionTableHtml : p === "[photoCredits]" ? photoCreditsHtml : `<p>${p}</p>`)).join("")).join("");

let activeChain = 1;
let activeStep = 0;

function buildSteps() {
  const host = document.getElementById("steps");
  host.innerHTML = "";
  const chain = CHAINS[activeChain];
  const roadLabel = COPY.roadName[activeChain];
  const routeVar = `style="--route:${ROUTE_COLOR[activeChain]}"`;
  // Bag End
  const first = document.createElement("article");
  first.className = "step"; first.dataset.step = "0";
  const be = COPY.bagEndSteps[activeChain];
  first.innerHTML = `<div class="step-inner"><h2>${be.title}</h2><p class="lede">${be.lede}</p><p>${be.body}</p></div>`;
  host.appendChild(first);
  milestones.forEach((m, i) => {
    const k = i + 1;
    const leg = chain.legs.find((l) => l.step === k);
    const stop = chain.ringStop[k];
    const detour = chain.detourStop[k];
    const places = (road().landmarksByRing.get(m.name) || []).slice(0, 3).map((d) => curly(d.label));
    const route = { road: roadLabel, stop: stop.label, detour: detour ? detour.label : null,
      legMi: leg ? leg.trueLegKm / KM_PER_MI : m.legKm / KM_PER_MI, targetMi: m.legKm / KM_PER_MI };
    let html = `<div class="step-inner"><div class="numeral">${ROMAN[k]}</div><h2>${m.display}</h2>`;
    html += `<p class="lede">${COPY.ringLede(m)}</p>`;
    html += `<p class="book"><span class="kicker">${COPY.bookKicker}</span>${COPY.bookEvents[k]}</p>`;
    html += `<p>${COPY.ringBody(m)} ${COPY.ringPlaces(places)}</p>`;
    if (m.name === "Amon Hen") html += `<p>${COPY.boatNote}</p>`;
    const note = (COPY.landmarkNotes[activeChain] || {})[k];
    const photo = photoFor(activeChain, k);
    if (photo) html += `<figure class="photo"><img src="photos/${photo.file}" alt="${esc(photo.alt || stop.label)}" width="${photo.width}" height="${photo.height}" loading="lazy"></figure>`;
    html += `<p class="route-line" ${routeVar}>${COPY.routeReach(route)}${note ? ` ${note}` : ""} ${COPY.routeLeg(route)}`;
    if (k === milestones.length) {
      const totalKm = d3.sum(chain.legs, (l) => l.trueLegKm);
      html += ` ${COPY.endLine({ road: roadLabel, totalMi: totalKm / KM_PER_MI, frodoMi: m.cumKm / KM_PER_MI })}`;
    }
    html += `</p></div>`;
    const el = document.createElement("article");
    el.className = "step"; el.dataset.step = String(k);
    el.innerHTML = html;
    host.appendChild(el);
  });
  observeSteps();
}

/* =====================================================================
   MAP
   ===================================================================== */
// Shire straw to Mordor umber: the land darkens on the approach to Mount Doom
const RING_COLORS = ["#e0bf6a", "#d9ae5a", "#d29d4a", "#c78b41", "#bb7a38", "#a96733",
                     "#98552d", "#824329", "#6d3224", "#53241d", "#3a1716"];
const mapSvg = d3.select("#map");
const toLine = (pts) => ({ type: "LineString", coordinates: pts.map((p) => [p.lon, p.lat]) });
let proj, geoPath, M = null, firstDraw = true, mapW = 0, mapH = 0, currentK = 1;

function addInkFilter(svg, id) {
  const f = svg.append("defs").append("filter").attr("id", id)
    .attr("x", "-5%").attr("y", "-5%").attr("width", "110%").attr("height", "110%");
  f.append("feTurbulence").attr("type", "fractalNoise").attr("baseFrequency", 0.04).attr("numOctaves", 2).attr("seed", 7).attr("result", "n");
  return f.append("feDisplacementMap").attr("in", "SourceGraphic").attr("in2", "n").attr("scale", 2.2);
}

// Mountain glyphs, thinned in screen space: tallest relief first, and none
// closer than GLYPH_SPACING pixels to one already drawn. Zooming in reveals more.
const GLYPH_SPACING = 17;
// Nudge each mountain off its 20 km grid cell so ranges don't line up in rows.
// A fixed seed keeps the nudges identical every time the map redraws.
const glyphRand = (() => {
  let a = 20240611;
  return () => {
    a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
})();
// Relief is capped so a few extreme cells don't shrink every other mountain
function makeGlyphSet(list, reliefCap) {
  const items = list
    .map(([lon, lat, r]) => [lon + (glyphRand() - 0.5) * 0.22, lat + (glyphRand() - 0.5) * 0.16, Math.min(r, reliefCap)])
    .sort((a, b) => b[2] - a[2]);
  return { items, maxRelief: d3.max(items, (g) => g[2]) };
}
const MAIN_GLYPHS = makeGlyphSet(DATA.glyphs, Infinity);
const CONT_GLYPHS = makeGlyphSet(CONT.glyphs, 2500);
function glyphPath(project, k, tx, ty, w, h, set = MAIN_GLYPHS) {
  const grid = new Map();
  // Mountains grow as the map zooms in, and need more room as they grow
  const grow = Math.min(2.2, Math.sqrt(k));
  const spacing = GLYPH_SPACING * grow;
  let s = "";
  for (const [lon, lat, relief] of set.items) {
    const p = project([lon, lat]);
    if (!p) continue;
    const sx = p[0] * k + tx, sy = p[1] * k + ty;
    if (sx < -20 || sy < -20 || sx > w + 20 || sy > h + 20) continue;
    const cx = Math.floor(sx / spacing), cy = Math.floor(sy / spacing);
    let free = true;
    for (let i = -1; i <= 1 && free; i++) {
      for (let j = -1; j <= 1 && free; j++) {
        const near = grid.get(`${cx + i},${cy + j}`);
        if (near) for (const [ax, ay] of near) {
          if ((ax - sx) ** 2 + (ay - sy) ** 2 < spacing ** 2) { free = false; break; }
        }
      }
    }
    if (!free) continue;
    const key = `${cx},${cy}`;
    if (!grid.has(key)) grid.set(key, []);
    grid.get(key).push([sx, sy]);
    const size = ((3 + 4.6 * Math.sqrt(Math.max(0, relief - 400) / (set.maxRelief - 400))) * grow) / k;
    const [x, y] = p, f = (v) => v.toFixed(2);
    s += `M${f(x - size)},${f(y)}L${f(x)},${f(y - size * 1.35)}L${f(x + size)},${f(y)}`;
    s += `M${f(x + size * 0.12)},${f(y - size)}L${f(x + size * 0.5)},${f(y)}`;
  }
  return s;
}

// Bridge symbol: two flared lines along the road, perpendicular to the river
const BRIDGE_PATH = "M-5,-3.4L-3.4,-1.7L3.4,-1.7L5,-3.4M-5,3.4L-3.4,1.7L3.4,1.7L5,3.4";
function bridgeAngles(project, rivers = DATA.rivers, bridges = DATA.bridges) {
  const segs = [];
  for (const f of rivers.features) {
    const geoms = f.geometry.type === "GeometryCollection" ? f.geometry.geometries : [f.geometry];
    const lines = geoms.flatMap((g) => g.type === "LineString" ? [g.coordinates] : g.type === "MultiLineString" ? g.coordinates : []);
    for (const line of lines) {
      const pts = line.map((c) => project(c)).filter(Boolean);
      for (let i = 1; i < pts.length; i++) segs.push([pts[i - 1], pts[i]]);
    }
  }
  return bridges.features.map((b) => {
    const p = project(b.geometry.coordinates);
    let best = null, bestD = Infinity;
    for (const [a, c] of segs) {
      const mx = (a[0] + c[0]) / 2, my = (a[1] + c[1]) / 2;
      const d = (mx - p[0]) ** 2 + (my - p[1]) ** 2;
      if (d < bestD) { bestD = d; best = [a, c]; }
    }
    const riverDeg = best ? (Math.atan2(best[1][1] - best[0][1], best[1][0] - best[0][0]) * 180) / Math.PI : 0;
    return { p, rot: riverDeg + 90, name: b.properties.name, river: b.properties.river };
  });
}

// Symbols keep their screen size while the map zooms: each is counter-scaled
function sym(sel, x, y, rot = 0) {
  return sel.classed("sym", true).attr("data-x", x).attr("data-y", y).attr("data-rot", rot);
}
function placeSyms(k) {
  M.z.selectAll(".sym").attr("transform", function () {
    return `translate(${this.dataset.x},${this.dataset.y}) scale(${1 / k}) rotate(${this.dataset.rot || 0})`;
  });
}

function drawMainMap() {
  const box = mapSvg.node().getBoundingClientRect();
  mapW = Math.max(320, box.width); mapH = Math.max(240, box.height);
  mapSvg.attr("viewBox", `0 0 ${mapW} ${mapH}`);
  mapSvg.selectAll("*").remove();
  const R = road(), BASE = R.base || CONT;
  proj = d3.geoAzimuthalEquidistant().rotate([-R.origin[0], -R.origin[1]]).precision(0.2)
    .fitExtent([[18, 18], [mapW - 18, mapH - 18]], R.rings);
  geoPath = d3.geoPath(proj);
  const disp = addInkFilter(mapSvg, "ink-main");
  mapSvg.append("rect").attr("class", "sea").attr("width", mapW).attr("height", mapH);
  const z = mapSvg.append("g").attr("class", "zoom");
  z.append("path").attr("class", "land").attr("d", geoPath(BASE.land)).attr("filter", "url(#ink-main)");
  const bands = z.append("g");
  // Nested rings become bands by cutting out the next ring in (even-odd fill)
  const ringById = new Map(R.rings.features.map((f) => [f.properties.milestoneId, f]));
  const bandPath = (d) => {
    if (!R.nested) return geoPath(d);
    const inner = ringById.get(d.properties.milestoneId - 1);
    return geoPath(d) + (inner ? geoPath(inner) : "");
  };
  bands.selectAll("path").data(R.rings.features).join("path").attr("class", "band").attr("fill-rule", "evenodd")
    .attr("fill", (d) => RING_COLORS[d.properties.milestoneId - 1]).attr("d", bandPath);
  z.append("path").attr("class", "lake").attr("d", geoPath(BASE.lakes));
  z.append("path").attr("class", "state").attr("d", geoPath(BASE.states));
  z.append("path").attr("class", "country").attr("d", geoPath(BASE.countries));
  const glyphs = z.append("path").attr("class", "glyphs");
  const lines = z.append("g");
  lines.selectAll("path").data(R.ringLines.features).join("path").attr("class", "ring-line").attr("d", geoPath);
  z.append("path").attr("class", "river").attr("d", geoPath(BASE.rivers));
  const bridgeG = z.append("g");
  for (const b of bridgeAngles(proj, BASE.rivers, BASE.bridges)) {
    sym(bridgeG.append("path").attr("class", "bridge").attr("d", BRIDGE_PATH), b.p[0], b.p[1], b.rot)
      .append("title").text(`${b.name} bridge over the ${b.river}`);
  }
  const ghostCasing = z.append("path").attr("class", "route-casing ghost");
  const ghost = z.append("path").attr("class", "route-ghost");
  const casing = z.append("path").attr("class", "route-casing");
  const route = z.append("path").attr("class", "route");
  const places = z.append("g");
  const stopsG = z.append("g");
  const be = proj(R.origin);
  sym(z.append("path").attr("class", "bagend").attr("d", d3.symbol(d3.symbolStar, 130)()), be[0], be[1])
    .append("title").text("Bag End");
  M = { z, disp, bands, lines, glyphs, ghostCasing, ghost, casing, route, places, stopsG };

  if (firstDraw && !reduceMotion) {
    bands.selectAll("path").style("opacity", 0)
      .transition("reveal").delay((d) => 250 + d.properties.milestoneId * 120).duration(700).style("opacity", 1);
  }
  firstDraw = false;
  updateMap(false);
}

// Frame the current ring and the road walked so far, so the map pulls back
// step by step as the hero travels farther from Bag End
function frameFor(k) {
  const f = road().rings.features.find((d) => d.properties.milestoneId === Math.max(1, k));
  let [[x0, y0], [x1, y1]] = geoPath.bounds(f);
  const chain = CHAINS[activeChain];
  const upTo = chain.stepIndex[k] ?? 0;
  if (upTo > 0) {
    const [[a0, b0], [a1, b1]] = geoPath.bounds(toLine(chain.profile.slice(0, upTo + 1)));
    x0 = Math.min(x0, a0); y0 = Math.min(y0, b0); x1 = Math.max(x1, a1); y1 = Math.max(y1, b1);
  }
  const fill = k === 0 ? 0.6 : 0.86;
  const s = Math.max(1, Math.min(6, fill / Math.max((x1 - x0) / mapW, (y1 - y0) / mapH)));
  const cx = (x0 + x1) / 2, cy = (y0 + y1) / 2;
  return d3.zoomIdentity.translate(mapW / 2 - s * cx, mapH / 2 - s * cy).scale(s);
}

function drawGlyphs(t) {
  M.disp.attr("scale", 2.2 / t.k);
  M.glyphs.attr("d", glyphPath(proj, t.k, t.x, t.y, mapW, mapH, road().glyphs()))
    .transition().duration(reduceMotion ? 0 : 350).attr("opacity", 1);
}

function zoomTo(t, animate) {
  M.glyphs.interrupt().attr("opacity", 0);
  if (!animate || reduceMotion) {
    M.z.interrupt().attr("transform", t.toString());
    currentK = t.k; placeSyms(t.k); drawGlyphs(t);
    return;
  }
  const fromK = currentK;
  M.z.transition("zoom").duration(950).ease(d3.easeCubicInOut)
    .attr("transform", t.toString())
    .tween("syms", () => (u) => { currentK = fromK + (t.k - fromK) * u; placeSyms(currentK); })
    .on("end", () => { currentK = t.k; placeSyms(t.k); drawGlyphs(t); });
}

function updateMap(animate = true) {
  if (!M) return;
  const k = activeStep;
  const chain = CHAINS[activeChain];
  const color = ROUTE_COLOR[activeChain];
  M.bands.selectAll("path").attr("fill-opacity", (d) => {
    const id = d.properties.milestoneId;
    if (id === k) return 0.92;
    if (id < k) return 0.8;
    return 0.16;
  });
  M.lines.selectAll("path").classed("is-active", (d) => d.properties.milestone === (milestones[k - 1] || {}).name);

  const prof = chain.profile;
  const ghostD = geoPath(toLine(prof));
  M.ghostCasing.attr("d", ghostD);
  M.ghost.attr("d", ghostD).style("stroke", color);
  const upTo = chain.stepIndex[k] ?? 0;
  const routeD = upTo > 0 ? geoPath(toLine(prof.slice(0, upTo + 1))) : null;
  M.casing.attr("d", routeD);
  M.route.attr("d", routeD).style("stroke", color);

  // Stops: every stop reached so far is a dot; only the current one gets a label
  M.stopsG.selectAll("*").remove();
  for (const s of chain.stops.filter((s) => s.step <= k && s.role !== "start")) {
    const p = proj(s.coords);
    const g = sym(M.stopsG.append("g"), p[0], p[1]);
    g.append("circle").attr("class", `stop ${s.role}`).attr("r", s.role === "ring" ? (s.step === k ? 5.5 : 3.6) : 2.6);
    g.append("title").text(s.label);
    if (s.step === k && s.role === "ring") g.append("text").attr("class", "label stop-label").attr("x", 9).attr("y", 4.5).text(s.label);
  }
  // Ring landmarks: dots, named on hover (the caption names them too)
  M.places.selectAll("*").remove();
  if (k > 0) {
    const m = milestones[k - 1];
    for (const d of (road().landmarksByRing.get(m.name) || []).slice(0, 4)) {
      if (chain.stops.some((s) => s.label === curly(d.label))) continue;
      const p = proj([d.lon, d.lat]);
      sym(M.places.append("circle").attr("class", "place-dot").attr("r", 3), p[0], p[1])
        .append("title").text(curly(d.label));
    }
  }
  document.getElementById("where").textContent = COPY.where(k === 0 ? "Bag End" : milestones[k - 1].display);
  updateProfile();
  updateLegs();
  zoomTo(frameFor(k), animate);
}

/* ---------- elevation strip ---------- */
const profSvg = d3.select("#profile");
function updateProfile() {
  const box = profSvg.node().getBoundingClientRect();
  const width = Math.max(300, box.width), height = Math.max(70, box.height);
  profSvg.attr("viewBox", `0 0 ${width} ${height}`);
  profSvg.selectAll("*").remove();
  const chain = CHAINS[activeChain];
  const prof = chain.profile;
  const m = { l: 44, r: 10, t: 18, b: 18 };
  const x = d3.scaleLinear().domain([0, d3.max(prof, (p) => p.d)]).range([m.l, width - m.r]);
  // Stagger numerals that would overlap: a crowded one moves up to a second row.
  // The second row only takes space when a road needs it (short legs sit close together).
  const ROW_GAP = 11;
  const labelW = (k) => ROMAN[k].length * 6.5 + 5;
  const marks = [];
  const lastRight = [-Infinity, -Infinity];
  for (let k = 1; k <= milestones.length; k++) {
    const p = prof[chain.stepIndex[k]];
    if (!p) continue;
    const cx = x(p.d), half = labelW(k) / 2;
    const row = cx - half >= lastRight[0] ? 0 : cx - half >= lastRight[1] ? 1 : 0;
    lastRight[row] = cx + half;
    marks.push({ k, p, cx, row });
  }
  if (marks.some((d) => d.row === 1)) m.t += ROW_GAP;
  const y = d3.scaleLinear().domain([0, Math.max(3000, d3.max(prof, (p) => p.e))]).nice().range([height - m.b, m.t]);
  profSvg.append("path").attr("class", "profile-area")
    .attr("d", d3.area().x((p) => x(p.d)).y0(height - m.b).y1((p) => y(p.e)).curve(d3.curveMonotoneX)(prof));
  profSvg.append("path").attr("class", "profile-line")
    .attr("d", d3.line().x((p) => x(p.d)).y((p) => y(p.e)).curve(d3.curveMonotoneX)(prof));
  const upTo = chain.stepIndex[activeStep] ?? 0;
  if (upTo > 0) {
    profSvg.append("path").attr("class", "profile-done").attr("stroke", ROUTE_COLOR[activeChain])
      .attr("d", d3.line().x((p) => x(p.d)).y((p) => y(p.e)).curve(d3.curveMonotoneX)(prof.slice(0, upTo + 1)));
  }
  for (const { k, cx, row } of marks) {
    const textY = m.t - 7 - row * ROW_GAP;
    // A raised numeral's tick reaches up to meet it
    profSvg.append("line").attr("class", "profile-tick").attr("x1", cx).attr("x2", cx).attr("y1", textY + 3).attr("y2", height - m.b);
    // Each numeral jumps to its step; the invisible box makes it easy to hit
    const jump = jumpTarget(profSvg.append("g").attr("class", "profile-jump").classed("is-here", k === activeStep), k);
    jump.append("rect").attr("class", "leg-hit").attr("x", cx - 11).attr("y", textY - 11).attr("width", 22).attr("height", 14);
    jump.append("text").attr("class", "profile-text").attr("x", cx).attr("y", textY).attr("text-anchor", "middle").text(ROMAN[k]);
  }
  // Keep axis labels at least 12 px apart (the phone strip is short)
  const yAxisVals = [];
  for (const v of y.ticks(2)) if (!yAxisVals.length || Math.abs(y(v) - y(yAxisVals.at(-1))) >= 12) yAxisVals.push(v);
  profSvg.selectAll(".ya").data(yAxisVals).join("text").attr("class", "profile-axis").attr("x", m.l - 6).attr("y", (v) => y(v) + 4)
    .attr("text-anchor", "end").text((v) => `${fmt(v)} ft`);
  profSvg.append("text").attr("class", "profile-axis").attr("x", width - m.r).attr("y", height - 3).attr("text-anchor", "end")
    .text(`${fmt(d3.max(prof, (p) => p.d))} miles`);
  const here = prof[upTo];
  profSvg.append("circle").attr("class", "walker").attr("r", 5).attr("fill", ROUTE_COLOR[activeChain])
    .attr("cx", x(here.d)).attr("cy", y(here.e));
}

// Make an SVG group act as a link to step k: click, Enter or Space
function jumpTarget(g, k) {
  return g.attr("tabindex", 0).attr("role", "link")
    .attr("aria-label", COPY.jumpTo(ROMAN[k], milestones[k - 1].display))
    .on("click", () => scrollToStep(k))
    .on("keydown", (event) => {
      if (event.key === "Enter" || event.key === " ") { event.preventDefault(); scrollToStep(k); }
    });
}

/* ---------- leg comparison: this road's walking effort beside Frodo's miles ---------- */
// One bar per leg in the road's color; Frodo's miles are an ink tick across it.
// Bars already walked are solid, the current one is strongest, the rest are faint.
const legsSvg = d3.select("#legs");
const legsTip = document.getElementById("legs-tip");
function legRows(chainId) {
  const chain = CHAINS[chainId];
  return milestones.map((m, i) => {
    const k = i + 1;
    const leg = chain.legs.find((l) => l.step === k);
    return { k, name: m.display, roadMi: (leg ? leg.trueLegKm : m.legKm) / KM_PER_MI, frodoMi: m.legKm / KM_PER_MI };
  });
}
function updateLegs() {
  const box = legsSvg.node().getBoundingClientRect();
  if (box.width < 10) return;  // hidden (the phone layout shows one strip at a time)
  const width = box.width, height = Math.max(50, box.height);
  legsSvg.attr("viewBox", `0 0 ${width} ${height}`);
  legsSvg.selectAll("*").remove();
  const rows = legRows(activeChain);
  const roadLabel = COPY.roadName[activeChain];
  const color = ROUTE_COLOR[activeChain];
  const narrow = width < 420;
  const m = { l: narrow ? 30 : 44, r: 10, t: 4, b: 16 };
  const x = d3.scaleBand().domain(rows.map((r) => r.k)).range([m.l, width - m.r]).paddingInner(0.3).paddingOuter(0.15);
  const y = d3.scaleLinear().domain([0, d3.max(rows, (r) => Math.max(r.roadMi, r.frodoMi))]).nice().range([height - m.b, m.t]);
  legsSvg.attr("aria-label", COPY.legsLabel(roadLabel));
  document.getElementById("legs-key").innerHTML =
    `<span style="--route:${color}"><span class="key-bar"></span>${COPY.legsKeyRoad(roadLabel)}</span>` +
    `<span><span class="key-tick"></span>${COPY.legsKeyFrodo}</span>`;
  // Recessive axis: two labels, no lines
  legsSvg.selectAll(".ya").data(y.ticks(2).filter((v) => v > 0)).join("text").attr("class", "profile-axis")
    .attr("x", m.l - 6).attr("y", (v) => y(v) + 4).attr("text-anchor", "end").text((v) => (narrow ? fmt(v) : `${fmt(v)} mi`));
  const barW = Math.min(18, x.bandwidth() * 0.62), tickW = Math.min(30, x.bandwidth());
  const r = Math.min(4, barW / 2);
  // A bar whose top corners are rounded and whose base sits flat on the baseline
  const barPath = (cx, top, base) => {
    const x0 = cx - barW / 2, x1 = cx + barW / 2, rr = Math.min(r, base - top);
    return `M${x0},${base}V${top + rr}Q${x0},${top} ${x0 + rr},${top}H${x1 - rr}Q${x1},${top} ${x1},${top + rr}V${base}Z`;
  };
  const base = y(0);
  for (const row of rows) {
    const cx = x(row.k) + x.bandwidth() / 2;
    const opacity = row.k === activeStep ? 1 : row.k < activeStep ? 0.7 : 0.3;
    const g = jumpTarget(legsSvg.append("g").attr("class", "profile-jump").classed("is-here", row.k === activeStep), row.k);
    g.append("path").attr("class", "leg-bar").attr("fill", color).attr("opacity", opacity).attr("d", barPath(cx, y(row.roadMi), base));
    g.append("line").attr("class", "leg-tick").attr("x1", cx - tickW / 2).attr("x2", cx + tickW / 2)
      .attr("y1", y(row.frodoMi)).attr("y2", y(row.frodoMi)).attr("opacity", row.k <= activeStep ? 1 : 0.55);
    g.append("text").attr("class", "profile-text").attr("x", cx).attr("y", height - 3).attr("text-anchor", "middle").text(ROMAN[row.k]);
    // Hit area: the whole column, bigger than the bar
    g.append("rect").attr("class", "leg-hit").attr("x", x(row.k) - (x.step() - x.bandwidth()) / 2).attr("y", 0)
      .attr("width", x.step()).attr("height", height)
      .on("pointerenter", () => showLegTip(row, cx, Math.min(y(row.roadMi), y(row.frodoMi))))
      .on("pointerleave", () => { legsTip.hidden = true; });
    g.on("focus", () => showLegTip(row, cx, Math.min(y(row.roadMi), y(row.frodoMi))))
      .on("blur", () => { legsTip.hidden = true; });
  }
  // The same numbers as a table, for screen readers
  const [h1, h2, h3] = COPY.legsTableHead;
  document.getElementById("legs-table").innerHTML = `<caption>${COPY.legsLabel(roadLabel)}</caption>` +
    `<thead><tr><th>${h1}</th><th>${h2}</th><th>${h3}</th></tr></thead><tbody>` +
    rows.map((row) => `<tr><td>${ROMAN[row.k]}, ${row.name}</td><td>${fmt(row.roadMi)}</td><td>${fmt(row.frodoMi)}</td></tr>`).join("") +
    `</tbody>`;
}
function showLegTip(row, cx, top) {
  const svgBox = legsSvg.node().getBoundingClientRect(), wrapBox = legsSvg.node().parentNode.getBoundingClientRect();
  legsTip.innerHTML = COPY.legsTip({ numeral: ROMAN[row.k], name: row.name, road: COPY.roadName[activeChain],
    roadMi: fmt(row.roadMi), frodoMi: fmt(row.frodoMi) });
  legsTip.hidden = false;
  const tipW = legsTip.offsetWidth, left = svgBox.left - wrapBox.left + cx;
  legsTip.style.left = `${Math.max(tipW / 2 + 4, Math.min(wrapBox.width - tipW / 2 - 4, left))}px`;
  legsTip.style.top = `${svgBox.top - wrapBox.top + top - 6}px`;
}

// The phone layout has room for one strip: elevation or legs
document.querySelectorAll(".strip-toggle button").forEach((btn) => {
  btn.textContent = btn.dataset.strip === "legs" ? COPY.stripLegs : COPY.stripElevation;
  btn.addEventListener("click", () => {
    document.querySelector(".stage").dataset.strip = btn.dataset.strip;
    document.querySelectorAll(".strip-toggle button").forEach((b) => b.setAttribute("aria-pressed", String(b === btn)));
    updateProfile(); updateLegs();
  });
});

/* ---------- scrolling ---------- */
const isPhone = () => window.matchMedia("(max-width: 860px)").matches;
// Bring step k to the line where the reader's eye is: mid-screen on a wide
// screen, just below the map on a phone
function scrollToStep(k, smooth = !reduceMotion) {
  const el = document.querySelector(`.step[data-step="${k}"]`);
  if (!el) return;
  const rect = el.getBoundingClientRect();
  const top = isPhone() ? rect.top + window.scrollY - window.innerHeight * 0.6
    : rect.top + window.scrollY - (window.innerHeight - Math.min(rect.height, window.innerHeight)) / 2;
  window.scrollTo({ top, behavior: smooth ? "smooth" : "auto" });
}
let observer = null;
function observeSteps() {
  if (observer) observer.disconnect();
  observer = new IntersectionObserver((entries) => {
    entries.forEach((e) => {
      if (e.isIntersecting) {
        const k = +e.target.dataset.step;
        if (k !== activeStep) { activeStep = k; updateMap(); writeJourneyHash(); }
        document.querySelectorAll(".step").forEach((s) => s.classList.toggle("is-active", +s.dataset.step === k));
      }
    });
  }, { rootMargin: window.matchMedia("(max-width: 860px)").matches ? "-76% 0px -22% 0px" : "-48% 0px -48% 0px", threshold: 0 });
  document.querySelectorAll(".step").forEach((s) => observer.observe(s));
}

function setChain(chainId, redraw = true) {
  activeChain = chainId;
  document.querySelectorAll(".roads button").forEach((b) => b.setAttribute("aria-pressed", String(+b.dataset.chain === chainId)));
  buildSteps();
  document.querySelectorAll(".step").forEach((s) => s.classList.toggle("is-active", +s.dataset.step === activeStep));
  if (redraw) drawMainMap();
}
document.querySelectorAll(".roads button").forEach((btn) => {
  btn.addEventListener("click", () => { setChain(+btn.dataset.chain); writeJourneyHash(); });
});

/* =====================================================================
   START YOUR OWN JOURNEY: walking-effort rings computed in the browser
   ===================================================================== */
const yoursSvg = d3.select("#yours-map");
const statusEl = document.getElementById("yours-status");
let yProj, yPath, yLayer;
let paceGrid = null;
let yoursOrigin = bagEnd;
let yoursContours = null;
let yoursRoad = null;
let yoursAvoid = new Set();
let yoursSeed = 0;
let yRoadLayer = null;
let runToken = 0;

// Meter-space projection matching the R grid (azimuthal equidistant on Bag End)
// Meter-space projection matching the continent grid: spherical Lambert
// azimuthal equal area, the same definition the R export used
const meterProj = d3.geoAzimuthalEqualArea()
  .rotate([-YMETA.projection.lon0, -YMETA.projection.lat0])
  .scale(YMETA.projection.radius).translate([0, 0]).precision(0.1);

async function loadPaceGrid() {
  if (paceGrid) return paceGrid;
  const bin = await loadPaceFile();  // gzip bytes, unpacked here so any web host can serve them
  const stream = new Blob([bin]).stream().pipeThrough(new DecompressionStream("gzip"));
  const bytes = new Uint8Array(await new Response(stream).arrayBuffer());
  const pace = new Float32Array(bytes.length);
  const span = YMETA.logPaceMax - YMETA.logPaceMin;
  for (let i = 0; i < bytes.length; i++) {
    const c = bytes[i];
    // 0 is impassable; every other code is a log-scaled walking pace
    pace[i] = c === 0 ? NaN : Math.exp(YMETA.logPaceMin + ((c - 1) / 254) * span);
  }
  paceGrid = pace;
  return pace;
}

class MinHeap {
  constructor(cap) { this.k = new Float64Array(cap); this.v = new Int32Array(cap); this.n = 0; this.topK = 0; }
  push(key, val) {
    if (this.n >= this.k.length) { const k2 = new Float64Array(this.k.length * 2); k2.set(this.k); this.k = k2; const v2 = new Int32Array(this.v.length * 2); v2.set(this.v); this.v = v2; }
    const k = this.k, v = this.v; let i = this.n++;
    while (i > 0) { const p = (i - 1) >> 1; if (k[p] <= key) break; k[i] = k[p]; v[i] = v[p]; i = p; }
    k[i] = key; v[i] = val;
  }
  pop() {
    const k = this.k, v = this.v;
    const topV = v[0]; this.topK = k[0];
    const n = --this.n; const lastK = k[n], lastV = v[n];
    let i = 0;
    while (true) {
      let c = 2 * i + 1; if (c >= n) break;
      if (c + 1 < n && k[c + 1] < k[c]) c++;
      if (k[c] >= lastK) break;
      k[i] = k[c]; v[i] = v[c]; i = c;
    }
    k[i] = lastK; v[i] = lastV;
    return topV;
  }
}

async function computeRings(lonLat, onProgress) {
  const pace = await loadPaceGrid();
  const { ncol, nrow, resM, xmin, ymax } = YMETA;
  const [mx, my] = meterProj(lonLat);
  const north = -my;
  const c0 = Math.floor((mx - xmin) / resM), r0 = Math.floor((ymax - north) / resM);
  if (c0 < 0 || r0 < 0 || c0 >= ncol || r0 >= nrow || !(pace[r0 * ncol + c0] > 0)) return null;

  const thresholds = milestones.map((m) => m.thresholdHours);
  const limit = d3.max(thresholds) * 1.1; // raw cost runs up to ~8% above corrected cost
  const N = ncol * nrow;
  const dist = new Float64Array(N).fill(Infinity);
  const heap = new MinHeap(1 << 18);
  const start = r0 * ncol + c0;
  dist[start] = 0; heap.push(0, start);
  const DR = [-1, -1, -1, 0, 0, 1, 1, 1], DC = [-1, 0, 1, -1, 1, -1, 0, 1];
  const W = [Math.SQRT2, 1, Math.SQRT2, 1, 1, Math.SQRT2, 1, Math.SQRT2].map((w) => w * resM);
  let rMin = r0, rMax = r0, cMin = c0, cMax = c0, settled = 0;
  while (heap.n > 0) {
    const u = heap.pop(); const du = heap.topK;
    if (du > dist[u]) continue;
    if (du > limit) break;
    const ur = (u / ncol) | 0, uc = u - ur * ncol;
    if (ur < rMin) rMin = ur; if (ur > rMax) rMax = ur; if (uc < cMin) cMin = uc; if (uc > cMax) cMax = uc;
    const pu = pace[u];
    for (let j = 0; j < 8; j++) {
      const r = ur + DR[j], c = uc + DC[j];
      if (r < 0 || c < 0 || r >= nrow || c >= ncol) continue;
      const nb = r * ncol + c; const pv = pace[nb];
      if (!(pv > 0)) continue;
      const nd = du + (pu + pv) * 0.5 * W[j];
      if (nd < dist[nb]) { dist[nb] = nd; heap.push(nd, nb); }
    }
    if (++settled % 40000 === 0) { onProgress(settled); await new Promise((res) => setTimeout(res, 0)); }
  }

  // Remove the octagon bias of 8-direction movement, then contour
  const pad = 2;
  const bx0 = Math.max(0, cMin - pad), by0 = Math.max(0, rMin - pad);
  const bx1 = Math.min(ncol - 1, cMax + pad), by1 = Math.min(nrow - 1, rMax + pad);
  const w = bx1 - bx0 + 1, h = by1 - by0 + 1;
  const values = new Float64Array(w * h).fill(-1e9);
  for (let r = by0; r <= by1; r++) {
    for (let c = bx0; c <= bx1; c++) {
      const d = dist[r * ncol + c];
      if (!isFinite(d)) continue;
      const dx = Math.abs(c - c0), dy = Math.abs(r - r0);
      const oct = Math.max(dx, dy) + (Math.SQRT2 - 1) * Math.min(dx, dy);
      const corr = oct > 0 ? Math.hypot(dx, dy) / oct : 1;
      values[(r - by0) * w + (c - bx0)] = -d * corr;
    }
  }
  const contours = d3.contours().size([w, h]).thresholds(thresholds.map((t) => -t))(values);
  const touchesEdge = bx0 === 0 || by0 === 0 || bx1 === ncol - 1 || by1 === nrow - 1;
  return { contours, bx0, by0, touchesEdge, dist, c0, r0 };
}

// Area of a contour ring in grid cells (shoelace formula)
function ringArea(ring) {
  let a = 0;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) a += (ring[j][0] + ring[i][0]) * (ring[j][1] - ring[i][1]);
  return Math.abs(a / 2);
}
const MIN_RING_CELLS = 60; // about 1,500 square km: drops loops around small lakes and islands

function contourToScreen(geom, bx0, by0) {
  const { resM, xmin, ymax } = YMETA;
  let s = "";
  for (const poly of geom.coordinates) {
    if (ringArea(poly[0]) < MIN_RING_CELLS) continue;
    for (const ring of poly) {
      if (ringArea(ring) < MIN_RING_CELLS) continue;
      let first = true;
      for (const [px, py] of ring) {
        const x = xmin + (bx0 + px) * resM, north = ymax - (by0 + py) * resM;
        const ll = meterProj.invert([x, -north]);
        const p = yProj(ll);
        if (!p) continue;
        s += (first ? "M" : "L") + p[0].toFixed(1) + "," + p[1].toFixed(1);
        first = false;
      }
      s += "Z";
    }
  }
  return s;
}


/* ---------- Your Journey: an estimated hero's road ----------------------
   The same picker as the main story, ported to the browser. It chooses one
   real landmark on every ring so each leg's walking effort roughly matches
   Frodo's miles, with a bonus for fame, a penalty for doubling back, and a
   penalty for straight lines across water or a river barrier. Legs use quick
   estimates rather than exact cost paths, so distances are approximate. */
const PICK = {
  tolKm: [25, 50, 90],       // how far off a ring a landmark may sit; widened only if a ring is empty
  slices: 12, perSlice: 5,    // candidates spread evenly around each ring
  wiggleKm: 2, famePenaltyKm: 50, detourMinSitelinks: 5, detourPoolSize: 1200,
  estimateScale: 1.11, turnPenaltyKm: 100, barrierPenaltyKm: 200, barrierSamples: 32,
  shakePenaltyKm: 150         // extra cost for landmarks an earlier road already used
};
// A repeatable pseudo-random number in [0, 1) for any integer, used to shake up fame
const hashRand = (n) => {
  let t = (n ^ 0x9e3779b9) >>> 0;
  t = Math.imul(t ^ (t >>> 16), 0x85ebca6b);
  t = Math.imul(t ^ (t >>> 13), 0xc2b2ae35);
  return ((t ^ (t >>> 16)) >>> 0) / 4294967296;
};
// Every landmark Your Journey can choose from, loaded the first time it is needed
let LMK = null;
const ensureLandmarks = lazy(async () => {
  const raw = await loadLandmarkFile();
  const out = [];
  for (let i = 0; i < raw.label.length; i++) {
    const [x, y] = meterProj([raw.lon[i], raw.lat[i]]);
    // The English Wikipedia title is the most reliable English name; drop any "(disambiguation)" tail
    out.push({ id: i, label: curly(raw.wikiTitle[i].replace(/\s*\([^)]*\)$/, "")), lon: raw.lon[i], lat: raw.lat[i], x, y: -y,
      sitelinks: raw.sitelinks[i], wiki: raw.wikiTitle[i], sentence: curly(raw.sentence[i]),
      settlement: raw.settlement ? !!raw.settlement[i] : false });
  }
  LMK = out;
  return out;
});

function hoursAtXY(res, x, north) {
  const { ncol, nrow, resM, xmin, ymax } = YMETA;
  const c = Math.floor((x - xmin) / resM), r = Math.floor((ymax - north) / resM);
  if (c < 0 || r < 0 || c >= ncol || r >= nrow) return NaN;
  const d = res.dist[r * ncol + c];
  if (!isFinite(d)) return NaN;
  const dx = Math.abs(c - res.c0), dy = Math.abs(r - res.r0);
  const oct = Math.max(dx, dy) + (Math.SQRT2 - 1) * Math.min(dx, dy);
  return d * (oct > 0 ? Math.hypot(dx, dy) / oct : 1);
}
// Does the straight line between two points cross water or a river barrier?
function crossesBarrier(ax, ay, bx, by) {
  const { ncol, nrow, resM, xmin, ymax } = YMETA;
  const n = PICK.barrierSamples;
  for (let i = 1; i < n; i++) {
    const t = i / n;
    const c = Math.floor((ax + t * (bx - ax) - xmin) / resM), r = Math.floor((ymax - (ay + t * (by - ay))) / resM);
    if (c < 0 || r < 0 || c >= ncol || r >= nrow || !(paceGrid[r * ncol + c] > 0)) return true;
  }
  return false;
}
const turnCost = (cos) => (isFinite(cos) ? PICK.turnPenaltyKm * Math.max(-cos, 0) : 0);

function pickRoad(res, opts = {}) {
  const avoid = opts.avoid || new Set();
  const seed = opts.seed || 0;
  // Each new road varies how much fame counts, and steers away from landmarks already used
  const jitter = (id) => (seed === 0 ? 1 : 0.65 + 0.7 * hashRand(id * 7919 + seed * 104729));
  const { resM, xmin, ymax } = YMETA;
  const speed = YMETA.flatSpeedKmh;
  const ox = xmin + (res.c0 + 0.5) * resM, oy = ymax - (res.r0 + 0.5) * resM;
  const cand = [];
  for (const L of LMK) {
    const h = hoursAtXY(res, L.x, L.y);
    if (!isFinite(h)) continue;
    const euc = Math.hypot(L.x - ox, L.y - oy) / 1000;
    cand.push({ L, h, ratio: euc > 5 ? Math.max((h * speed) / euc, 1) : 1,
      fame: (PICK.famePenaltyKm / Math.log2(1 + L.sitelinks)) * jitter(L.id) + (avoid.has(L.id) ? PICK.shakePenaltyKm : 0),
      bearing: ((Math.atan2(L.x - ox, L.y - oy) * 180) / Math.PI + 360) % 360 });
  }
  const T = milestones.map((m) => m.thresholdHours);
  const layers = [];
  for (let k = 0; k < T.length; k++) {
    let near = [];
    for (const tol of PICK.tolKm) {
      near = cand.filter((c) => Math.abs(c.h - T[k]) * speed <= tol);
      if (near.length >= 3) break;
    }
    if (!near.length) break; // no landmarks on this ring: the road ends at the previous one
    const notTowns = near.filter((c) => !c.L.settlement);
    if (notTowns.length >= 3) near = notTowns; // towns only where nothing else fits
    near.sort((a, b) => b.L.sitelinks - a.L.sitelinks);
    const perSlice = new Map(), pick = [];
    for (const c of near) {
      const s = Math.floor(c.bearing / (360 / PICK.slices));
      const used = perSlice.get(s) || 0;
      if (used < PICK.perSlice) { perSlice.set(s, used + 1); pick.push(c); }
    }
    layers.push(pick);
  }
  if (!layers.length) return null;

  const estKm = (a, b) => (Math.hypot(a.x - b.x, a.y - b.y) / 1000) * ((a.ratio + b.ratio) / 2) * PICK.estimateScale;
  const median = (v) => { const s = v.slice().sort((a, b) => a - b); return s[Math.floor(s.length / 2)]; };
  let prev = [{ x: ox, y: oy, ratio: median(layers[0].map((c) => c.ratio)), id: -1, inX: 0, inY: 0, cost: 0 }];
  const history = [];
  for (let k = 0; k < layers.length; k++) {
    const B = layers[k].map((c) => ({ ...c, x: c.L.x, y: c.L.y, id: c.L.id }));
    const target = milestones[k].legKm;
    const lowH = k === 0 ? 0 : T[k - 1] * 0.8;
    const pool = cand.filter((c) => !c.L.settlement && c.L.sitelinks >= PICK.detourMinSitelinks && c.h >= lowH && c.h <= T[k])
      .sort((a, b) => b.L.sitelinks - a.L.sitelinks).slice(0, PICK.detourPoolSize)
      .map((c) => ({ ...c, x: c.L.x, y: c.L.y, id: c.L.id }));
    // Pool-to-ring quantities don't depend on where the hero came from, so compute them once
    const PB = pool.map((p) => B.map((b) => ({
      est: estKm(p, b), blocked: crossesBarrier(p.x, p.y, b.x, b.y) ? PICK.barrierPenaltyKm : 0,
      dx: b.x - p.x, dy: b.y - p.y })));
    const next = B.map(() => ({ cost: Infinity }));
    prev.forEach((a, ai) => {
      const AP = pool.map((p) => {
        const dx = p.x - a.x, dy = p.y - a.y, len = Math.hypot(dx, dy);
        return { est: estKm(a, p), dx, dy, len,
          extra: p.fame + turnCost((a.inX * dx + a.inY * dy) / len) +
            (crossesBarrier(a.x, a.y, p.x, p.y) ? PICK.barrierPenaltyKm : 0) };
      });
      B.forEach((b, bi) => {
        if (b.id === a.id) return; // never stay on the same landmark
        const dx = b.x - a.x, dy = b.y - a.y;
        const direct = estKm(a, b);
        let best = Math.max(Math.abs(direct - target) - PICK.wiggleKm, 0) +
          turnCost((a.inX * dx + a.inY * dy) / Math.hypot(dx, dy)) +
          (crossesBarrier(a.x, a.y, b.x, b.y) ? PICK.barrierPenaltyKm : 0);
        let detour = null, legEst = direct;
        for (let pi = 0; pi < pool.length; pi++) {
          const p = pool[pi];
          if (p.id === b.id || p.id === a.id) continue;
          const ap = AP[pi], pb = PB[pi][bi];
          const via = ap.est + pb.est;
          const miss = Math.max(Math.abs(via - target) - PICK.wiggleKm, 0);
          if (miss + ap.extra >= best) continue;
          const c = miss + ap.extra + pb.blocked +
            turnCost((ap.dx * pb.dx + ap.dy * pb.dy) / (ap.len * Math.hypot(pb.dx, pb.dy)));
          if (c < best) { best = c; detour = p; legEst = via; }
        }
        const total = a.cost + best + b.fame;
        if (total < next[bi].cost) {
          const fx = detour ? detour.x : a.x, fy = detour ? detour.y : a.y;
          const len = Math.hypot(b.x - fx, b.y - fy) || 1;
          next[bi] = { cost: total, from: ai, detour, legEst, inX: (b.x - fx) / len, inY: (b.y - fy) / len };
        }
      });
    });
    history.push(next);
    prev = B.map((b, bi) => ({ ...b, ...next[bi] }));
  }
  // Trace back from the best final landmark
  let j = prev.reduce((best, p, i) => (p.cost < prev[best].cost ? i : best), 0);
  const stops = [];
  for (let k = layers.length - 1; k >= 0; k--) {
    const node = history[k][j];
    stops.unshift({ k, landmark: layers[k][j].L, detour: node.detour ? node.detour.L : null, legEstKm: node.legEst });
    j = node.from;
  }
  return { stops, reachedAll: layers.length === milestones.length };
}

function renderRoad() {
  yRoadLayer.selectAll("*").remove();
  if (!yoursRoad) return;
  const g = yRoadLayer;
  const pts = [yoursOrigin];
  for (const s of yoursRoad.stops) {
    if (s.detour) pts.push([s.detour.lon, s.detour.lat]);
    pts.push([s.landmark.lon, s.landmark.lat]);
  }
  const xy = pts.map((p) => yProj(p)).filter(Boolean);
  const line = d3.line().curve(d3.curveCatmullRom.alpha(0.5));
  g.append("path").attr("class", "route-casing").attr("d", line(xy));
  g.append("path").attr("class", "route").style("stroke", "var(--blood)").attr("d", line(xy));
  for (const s of yoursRoad.stops) {
    if (s.detour) {
      const p = yProj([s.detour.lon, s.detour.lat]);
      g.append("circle").attr("class", "stop detour").attr("r", 2.4).attr("cx", p[0]).attr("cy", p[1])
        .append("title").text(s.detour.label);
    }
    const p = yProj([s.landmark.lon, s.landmark.lat]);
    g.append("circle").attr("class", "stop ring").attr("r", 4).attr("cx", p[0]).attr("cy", p[1])
      .append("title").text(`${ROMAN[s.k + 1]}. ${s.landmark.label}`);
  }
}

function renderRoadCards() {
  const host = d3.select("#yours-road");
  host.selectAll("*").remove();
  if (!yoursRoad) return;
  host.append("h3").text(COPY.yoursRoadTitle);
  host.append("p").attr("class", "road-intro").text(COPY.yoursRoadIntro);
  const list = host.append("ol").attr("class", "road-list");
  // The starting point gets its own entry, so a searched place is part of the story
  const start = list.append("li").attr("class", "road-start");
  start.append("div").attr("class", "numeral").text(COPY.originHeading);
  const startTitle = start.append("h4");
  const info = yoursOriginInfo;
  if (info.landmark) {
    startTitle.append("a").attr("href", `https://en.wikipedia.org/wiki/${encodeURIComponent(info.landmark.wiki.replace(/ /g, "_"))}`)
      .attr("target", "_blank").attr("rel", "noopener").text(info.name);
    start.append("p").attr("class", "wiki").text(info.landmark.sentence)
      .append("span").attr("class", "credit").text(" From Wikipedia");
  } else {
    startTitle.text(info.name);
    if (info.sub) start.append("p").attr("class", "wiki").text(info.sub);
  }
  start.append("p").attr("class", "lede").text(COPY.originLine);
  let walkedKm = 0;
  for (const s of yoursRoad.stops) {
    walkedKm += s.legEstKm;
    const m = milestones[s.k];
    const fromHomeMi = d3.geoDistance(yoursOrigin, [s.landmark.lon, s.landmark.lat]) * 3958.8;
    const li = list.append("li");
    li.append("div").attr("class", "numeral").text(`${ROMAN[s.k + 1]} · ${m.display}`);
    const h = li.append("h4");
    h.append("a").attr("href", `https://en.wikipedia.org/wiki/${encodeURIComponent(s.landmark.wiki.replace(/ /g, "_"))}`)
      .attr("target", "_blank").attr("rel", "noopener").text(s.landmark.label);
    li.append("p").attr("class", "wiki").text(s.landmark.sentence)
      .append("span").attr("class", "credit").text(" From Wikipedia");
    li.append("p").attr("class", "lede").text(COPY.yoursStopLine({ walkedMi: walkedKm / KM_PER_MI, fromHomeMi,
      frodoMi: m.cumKm / KM_PER_MI, detour: s.detour ? s.detour.label : null }));
    li.append("p").attr("class", "book").html(`<span class="kicker">${COPY.bookKicker}</span>`)
      .append("span").text(COPY.bookEvents[s.k + 1]);
  }
  if (!yoursRoad.reachedAll) {
    host.append("p").attr("class", "road-intro").text(COPY.yoursRoadShort(milestones[yoursRoad.stops.length - 1].display));
  }
  // Your journey beside Frodo's, measured at the last milestone your road reached
  const last = yoursRoad.stops[yoursRoad.stops.length - 1];
  const fm = milestones[last.k];
  const totalMi = walkedKm / KM_PER_MI;
  const endMi = d3.geoDistance(yoursOrigin, [last.landmark.lon, last.landmark.lat]) * 3958.8;
  const yourLongest = yoursRoad.stops.reduce((a, b) => (b.legEstKm > a.legEstKm ? b : a));
  const frodoLongest = milestones.slice(0, last.k + 1).reduce((a, b) => (b.legKm > a.legKm ? b : a));
  const rows = [
    [COPY.statWalked, `${fmt(totalMi)} miles`, `${fmt(fm.cumKm / KM_PER_MI)} miles`],
    [COPY.statFromHome, `${fmt(endMi)} miles`, `about ${fmt(fm.displacementMi)} miles`],
    [COPY.statWander, (totalMi / endMi).toFixed(1), (fm.cumKm / KM_PER_MI / fm.displacementMi).toFixed(1)],
    [COPY.statLongest, `${fmt(yourLongest.legEstKm / KM_PER_MI)} miles, to ${milestones[yourLongest.k].display}`,
      `${fmt(frodoLongest.legKm / KM_PER_MI)} miles, to ${frodoLongest.display}`],
    [COPY.statLandmarks, String(yoursRoad.stops.length + yoursRoad.stops.filter((s) => s.detour).length), `${last.k + 1} milestones`],
    [COPY.statEnd, last.landmark.label, fm.display]
  ];
  host.append("h3").attr("class", "stats-title").text(COPY.yoursStatsTitle);
  const table = host.append("table").attr("class", "friction stats");
  const head = table.append("thead").append("tr");
  head.append("th").text(""); head.append("th").text(COPY.statYou); head.append("th").text(COPY.statFrodo);
  const body = table.append("tbody");
  for (const r of rows) {
    const tr = body.append("tr");
    r.forEach((v) => tr.append("td").text(v));
  }
}

function drawYoursMap() {
  const box = yoursSvg.node().getBoundingClientRect();
  const width = Math.max(320, box.width), height = Math.max(240, box.height);
  yoursSvg.attr("viewBox", `0 0 ${width} ${height}`);
  yoursSvg.selectAll("*").remove();
  yProj = d3.geoAzimuthalEqualArea().rotate([-YMETA.projection.lon0, -YMETA.projection.lat0]).precision(0.2)
    .fitExtent([[10, 10], [width - 10, height - 10]], CONT.land);
  yPath = d3.geoPath(yProj);
  addInkFilter(yoursSvg, "ink-yours");
  yoursSvg.append("rect").attr("class", "sea").attr("width", width).attr("height", height);
  yoursSvg.append("path").attr("class", "land").attr("d", yPath(CONT.land)).attr("filter", "url(#ink-yours)");
  yLayer = yoursSvg.append("g");
  yoursSvg.append("path").attr("class", "lake").attr("d", yPath(CONT.lakes));
  yoursSvg.append("path").attr("class", "state").attr("d", yPath(CONT.states));
  yoursSvg.append("path").attr("class", "country").attr("d", yPath(CONT.countries));
  yoursSvg.append("path").attr("class", "glyphs").attr("d", glyphPath(yProj, 1, 0, 0, width, height, CONT_GLYPHS));
  yoursSvg.append("path").attr("class", "river").attr("d", yPath(CONT.rivers));
  for (const b of bridgeAngles(yProj, CONT.rivers, CONT.bridges)) {
    yoursSvg.append("path").attr("class", "bridge").attr("d", BRIDGE_PATH)
      .attr("transform", `translate(${b.p[0]},${b.p[1]}) rotate(${b.rot})`)
      .append("title").text(`${b.name} bridge over the ${b.river}`);
  }
  yRoadLayer = yoursSvg.append("g").attr("class", "your-road");
  yoursSvg.append("g").attr("class", "start-pin");
  renderYours();
}

function renderYours() {
  if (!yLayer) return;
  yLayer.selectAll("*").remove();
  if (yoursContours) {
    const { contours, bx0, by0 } = yoursContours;
    // d3 returns contours sorted by value, so match each back to its milestone,
    // then paint the outermost first so each nearer ring sits on top
    const byMilestone = contours.map((c) => ({ c, i: milestones.findIndex((m) => Math.abs(-m.thresholdHours - c.value) < 1e-6) }))
      .filter((d) => d.i >= 0).sort((a, b) => b.i - a.i);
    for (const { c, i } of byMilestone) {
      yLayer.append("path").attr("class", "yours-band").attr("fill-rule", "evenodd")
        .attr("fill", RING_COLORS[i]).attr("d", contourToScreen(c, bx0, by0))
        .append("title").text(milestones[i].display);
    }
  }
  renderRoad();
  const pin = yoursSvg.select(".start-pin");
  pin.selectAll("*").remove();
  const p = yProj(yoursOrigin);
  if (p) pin.append("path").attr("class", "bagend").attr("transform", `translate(${p})`).attr("d", d3.symbol(d3.symbolStar, 140)());
}

// What the Your Road list says about the starting point
const DEFAULT_ORIGIN_INFO = { name: "Data Trust office", sub: "Arlington, Virginia" };
let yoursOriginInfo = DEFAULT_ORIGIN_INFO;
async function runYours(lonLat, info = null) {
  const token = ++runToken;
  document.getElementById("yours-shake").disabled = true;
  statusEl.textContent = COPY.yoursWorking(0);
  const reached = 320000; // rough count of land cells within reach, for the progress readout
  let result;
  try {
    const landmarksReady = ensureLandmarks();  // fetch alongside the ring computation
    result = await computeRings(lonLat, (n) => {
      if (token === runToken) statusEl.textContent = COPY.yoursWorking(Math.min(99, Math.round((n / reached) * 100)));
    });
    await landmarksReady;
  } catch (err) {
    statusEl.textContent = "The rings could not be computed in this browser.";
    return;
  }
  if (token !== runToken) return;
  if (!result) { statusEl.textContent = COPY.yoursWater; return; }
  yoursOrigin = lonLat;
  yoursOriginInfo = info || DEFAULT_ORIGIN_INFO;
  writeYoursHash();
  yoursContours = result;
  yoursRoad = null;
  renderYours();
  renderRoadCards();
  statusEl.textContent = COPY.yoursPicking;
  await new Promise((r) => setTimeout(r, 30));
  if (token !== runToken) return;
  yoursAvoid = new Set(); yoursSeed = 0;
  try { yoursRoad = pickRoad(result, { avoid: yoursAvoid, seed: yoursSeed }); } catch (err) { yoursRoad = null; }
  document.getElementById("yours-shake").disabled = !yoursRoad;
  if (token !== runToken) return;
  renderYours();
  renderRoadCards();
  statusEl.textContent = lonLat === bagEnd ? COPY.yoursReady : (result.touchesEdge ? COPY.yoursEdge : COPY.yoursDone);
}

yoursSvg.on("click", (event) => {
  const [x, y] = d3.pointer(event, yoursSvg.node());
  const ll = yProj.invert([x, y]);
  if (ll) runYours(ll, { name: COPY.originClicked, sub: `${ll[1].toFixed(2)}, ${ll[0].toFixed(2)}` });
});
document.getElementById("yours-reset").addEventListener("click", () => runYours(bagEnd));

// Start from wherever the reader is. The browser asks their permission first.
(() => {
  const btn = document.getElementById("yours-locate");
  if (!("geolocation" in navigator)) { btn.parentNode.hidden = true; return; }
  btn.addEventListener("click", () => {
    statusEl.textContent = COPY.locating;
    btn.disabled = true;
    navigator.geolocation.getCurrentPosition((pos) => {
      btn.disabled = false;
      document.getElementById("yours-map-wrap").scrollIntoView({ block: "center", behavior: reduceMotion ? "auto" : "smooth" });
      // private: a shared link only carries this point rounded to about 10 km
      runYours([pos.coords.longitude, pos.coords.latitude], { name: COPY.originLocated, sub: "", private: true });
    }, (err) => {
      btn.disabled = false;
      statusEl.textContent = err.code === err.PERMISSION_DENIED ? COPY.locateDenied : COPY.locateFailed;
    }, { enableHighAccuracy: false, timeout: 15000, maximumAge: 600000 });
  });
})();

/* ---------- search for a starting point ---------- */
// Towns and cities (once a places list is embedded) plus every landmark,
// matched on all typed words, ignoring accents and case.
const normalizeText = (str) => String(str).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase();
let SEARCH_INDEX = null;
const ensureSearchIndex = lazy(async () => {
  const [P, landmarks] = await Promise.all([loadPlacesFile(), ensureLandmarks()]);
  const items = [];
  for (let i = 0; i < P.n.length; i++) {
    items.push({ name: P.n[i], sub: P.r[i], lon: P.lon[i], lat: P.lat[i], rank: Math.log10(P.p[i] + 1) * 10 });
  }
  for (const L of landmarks) items.push({ name: L.label, sub: COPY.searchLandmark, lon: L.lon, lat: L.lat, rank: L.sitelinks, landmark: L });
  for (const it of items) { it.key = normalizeText(`${it.name} ${it.sub}`); it.nameKey = normalizeText(it.name); }
  SEARCH_INDEX = items;
  return items;
});
function searchPlaces(query) {
  const coord = query.match(/^\s*(-?\d+(?:\.\d+)?)\s*[, ]\s*(-?\d+(?:\.\d+)?)\s*$/);
  if (coord) {
    const lat = +coord[1], lon = +coord[2];
    if (Math.abs(lat) <= 90 && Math.abs(lon) <= 180) return [{ name: `${lat}, ${lon}`, sub: COPY.searchCoords, lon, lat }];
  }
  const q = normalizeText(query.trim());
  if (q.length < 2 || !SEARCH_INDEX) return [];
  const words = q.split(/\s+/);
  const scored = [];
  for (const it of SEARCH_INDEX) {
    if (!words.every((w) => it.key.includes(w))) continue;
    const bonus = it.nameKey === q ? 1000 : it.nameKey.startsWith(q) ? 500 : 0;
    scored.push([bonus + it.rank, it]);
  }
  return scored.sort((a, b) => b[0] - a[0]).slice(0, 8).map((x) => x[1]);
}
(() => {
  const input = document.getElementById("yours-search");
  const list = document.getElementById("yours-results");
  input.placeholder = COPY.searchPlaceholder;
  let results = [], active = -1;
  const close = () => { list.hidden = true; input.setAttribute("aria-expanded", "false"); active = -1; };
  const choose = (it) => {
    input.value = it.sub && it.sub !== COPY.searchCoords ? `${it.name}, ${it.sub}` : it.name;
    close();
    document.getElementById("yours-map-wrap").scrollIntoView({ block: "center", behavior: reduceMotion ? "auto" : "smooth" });
    runYours([it.lon, it.lat], { name: it.name, sub: it.landmark ? "" : it.sub, landmark: it.landmark || null });
  };
  const render = () => {
    list.innerHTML = "";
    if (!input.value.trim()) { close(); return; }
    if (!results.length) {
      const li = document.createElement("li"); li.className = "empty"; li.textContent = COPY.searchNone; list.appendChild(li);
    }
    results.forEach((it, i) => {
      const li = document.createElement("li");
      li.setAttribute("role", "option"); li.id = `yours-result-${i}`;
      li.setAttribute("aria-selected", String(i === active));
      li.textContent = it.name;
      if (it.sub) { const span = document.createElement("span"); span.className = "sub"; span.textContent = it.sub; li.appendChild(span); }
      li.addEventListener("mousedown", (e) => { e.preventDefault(); choose(it); });
      list.appendChild(li);
    });
    list.hidden = false; input.setAttribute("aria-expanded", "true");
    if (active >= 0) input.setAttribute("aria-activedescendant", `yours-result-${active}`); else input.removeAttribute("aria-activedescendant");
  };
  const update = () => { results = searchPlaces(input.value); active = results.length ? 0 : -1; render(); };
  // The place list loads the first time the reader focuses or types in the box
  const warm = () => ensureSearchIndex().then(update).catch(() => { statusEl.textContent = COPY.searchFailed; });
  input.addEventListener("focus", warm);
  input.addEventListener("input", () => (SEARCH_INDEX ? update() : warm()));
  input.addEventListener("keydown", (e) => {
    if (e.key === "ArrowDown" && results.length) { active = (active + 1) % results.length; render(); e.preventDefault(); }
    else if (e.key === "ArrowUp" && results.length) { active = (active - 1 + results.length) % results.length; render(); e.preventDefault(); }
    else if (e.key === "Enter" && active >= 0 && results[active]) { choose(results[active]); e.preventDefault(); }
    else if (e.key === "Escape") { close(); }
  });
  input.addEventListener("blur", () => setTimeout(close, 120));
})();
// Same rings, a different road: earlier landmarks cost more and fame is reshuffled
document.getElementById("yours-shake").addEventListener("click", async () => {
  if (!yoursContours || !yoursRoad) return;
  for (const st of yoursRoad.stops) { yoursAvoid.add(st.landmark.id); if (st.detour) yoursAvoid.add(st.detour.id); }
  yoursSeed += 1;
  statusEl.textContent = COPY.yoursPicking;
  await new Promise((r) => setTimeout(r, 30));
  try { yoursRoad = pickRoad(yoursContours, { avoid: yoursAvoid, seed: yoursSeed }); } catch (err) { return; }
  renderYours();
  renderRoadCards();
  statusEl.textContent = COPY.yoursShaken;
});
document.getElementById("yours-legend").innerHTML = milestones
  .map((m, i) => `<span style="--c:${RING_COLORS[i]}">${m.display}</span>`).join("");

/* ---------- boot ---------- */
/* ---------- tabs: one link per tab, routed on the URL hash ---------- */
const TABS = ["journey", "yours", "method"];
let activeTab = null;
function showTab(name) {
  if (!TABS.includes(name)) name = "journey";
  if (name === activeTab) return;
  activeTab = name;
  document.querySelectorAll(".tab-panel").forEach((p) => { p.hidden = p.dataset.panel !== name; });
  document.querySelectorAll(".tabs a").forEach((a) => {
    if (a.dataset.tab === name) a.setAttribute("aria-current", "page"); else a.removeAttribute("aria-current");
  });
  // Maps can only measure themselves once visible, so draw after the panel shows
  requestAnimationFrame(() => {
    if (name === "journey") drawMainMap();
    if (name === "yours") drawYoursMap();
  });
}
function drawAll() {
  if (activeTab === "journey") drawMainMap();
  if (activeTab === "yours") drawYoursMap();
}
/* ---------- links that open a road, a step, or a starting point ----------
   #journey/2/7                      Hobbit Trail road, step VII
   #yours/44.170,-124.105/Hobbit%20Trail   Your Journey from that point, with its name
   The address updates quietly as the reader moves, so it can be copied at any time. */
function parseHash(hash = location.hash) {
  const [tab, ...rest] = hash.replace(/^#/, "").split("/");
  if (tab === "journey") {
    const road = rest[0] === "2" ? 2 : 1;
    const step = Math.max(0, Math.min(milestones.length, parseInt(rest[1], 10) || 0));
    return { tab, road, step, explicit: rest.length > 0 };
  }
  if (tab === "yours" && rest[0]) {
    const [lat, lon] = rest[0].split(",").map(Number);
    if (Math.abs(lat) <= 90 && Math.abs(lon) <= 180) {
      return { tab, lonLat: [lon, lat], name: rest[1] ? decodeURIComponent(rest[1]) : null };
    }
  }
  return { tab: TABS.includes(tab) ? tab : "journey" };
}
function writeHash(hash) {
  if (location.hash !== hash) history.replaceState(null, "", hash);
}
function writeJourneyHash() {
  if (activeTab !== "journey") return;
  writeHash(activeChain === 1 && activeStep === 0 ? "#journey" : `#journey/${activeChain}/${activeStep}`);
}
function writeYoursHash() {
  if (activeTab !== "yours") return;
  if (yoursOrigin === bagEnd) { writeHash("#yours"); return; }
  const info = yoursOriginInfo || {};
  const digits = info.private ? 1 : 3;
  const [lon, lat] = yoursOrigin;
  const name = info.private ? "" : info.name && info.name !== COPY.originClicked ? `/${encodeURIComponent(info.name)}` : "";
  writeHash(`#yours/${lat.toFixed(digits)},${lon.toFixed(digits)}${name}`);
}
// A named start from a link: if it is a landmark, bring back its Wikipedia line too
async function originInfoFor(route) {
  const [lon, lat] = route.lonLat;
  const coords = `${lat.toFixed(2)}, ${lon.toFixed(2)}`;
  if (!route.name) return { name: COPY.originClicked, sub: coords };
  try {
    const landmarks = await ensureLandmarks();
    const L = landmarks.find((d) => d.label === route.name && Math.abs(d.lon - lon) < 0.01 && Math.abs(d.lat - lat) < 0.01);
    if (L) return { name: L.label, sub: "", landmark: L };
  } catch (err) { /* the name alone is fine */ }
  return { name: route.name, sub: coords };
}

let yoursStarted = false;
let pendingYours = null;
async function startYours(route) {
  yoursStarted = true;
  if (route && route.lonLat) runYours(route.lonLat, await originInfoFor(route));
  else runYours(bagEnd);
}
function applyRoute(route, { initial = false } = {}) {
  showTab(route.tab);
  if (route.tab === "journey" && route.explicit) {
    activeStep = route.step;
    if (route.road !== activeChain) setChain(route.road, !initial);
    else document.querySelectorAll(".step").forEach((s) => s.classList.toggle("is-active", +s.dataset.step === activeStep));
    // After the panel is visible and laid out, put the step where the reader is looking
    requestAnimationFrame(() => requestAnimationFrame(() => { scrollToStep(route.step, !initial && !reduceMotion); if (!initial) updateMap(); }));
    if (initial) {
      // Web fonts arriving can push the steps down a little; settle on the step again
      // once they have, unless the reader has already started scrolling
      let touched = false;
      const mark = () => { touched = true; };
      ["wheel", "touchstart", "keydown", "pointerdown"].forEach((t) => window.addEventListener(t, mark, { once: true, passive: true }));
      const settle = () => { if (!touched) scrollToStep(route.step, false); };
      document.fonts.ready.then(settle);
      window.addEventListener("load", settle, { once: true });
    }
    return;
  }
  if (route.tab === "yours" && route.lonLat) {
    if (yoursStarted) startYours(route); else pendingYours = route;
  }
  if (!initial) document.querySelector(".tabs").scrollIntoView({ block: "start" });
}
window.addEventListener("hashchange", () => applyRoute(parseHash()));

buildSteps();
applyRoute(parseHash(), { initial: true });
let resizeTimer;
window.addEventListener("resize", () => { clearTimeout(resizeTimer); resizeTimer = setTimeout(drawAll, 200); });
// Start the in-browser computation once the reader approaches that section
const yoursObs = new IntersectionObserver((entries) => {
  if (entries.some((e) => e.isIntersecting)) { yoursObs.disconnect(); startYours(pendingYours); pendingYours = null; }
}, { rootMargin: "600px 0px" });
yoursObs.observe(document.getElementById("yours-map-wrap"));
// Once the story is drawn, fetch Your Journey's files in the background so that tab opens quickly
setTimeout(() => { loadPaceFile(); ensureSearchIndex().catch(() => {}); }, 2500);

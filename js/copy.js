/* =====================================================================
   COPY: every word on the page lives here, so it can be edited in one
   place. Captions are factual drafts describing what the map shows.
   ===================================================================== */
/* Number and list formatting used in the wording below */
export const fmt = (n) => Math.round(n).toLocaleString("en-US");
export function listify(a) {
  if (a.length <= 1) return a.join("");
  if (a.length === 2) return `${a[0]} and ${a[1]}`;
  return `${a.slice(0, -1).join(", ")}, and ${a[a.length - 1]}`;
}

export const COPY = {
  title: "Mount Doom Is Closer Than You Think",
  subtitle: "Frodo’s journey from the Shire to Mount Doom, measured in walking effort from Arlington, Virginia.",
  intro: [
    "Put Bag End at the Data Trust office, 1310 North Courthouse Road in Arlington, and send a hobbit walking. Each ring on the map below marks how far Frodo had traveled from home at one milestone of his journey, measured not in straight miles but in the effort it takes to walk there.",
    "Effort comes from a walking model. Steep slopes slow a walker down, forests and wetlands slow them more, thin air above about 4,900 feet slows them a little, and the Mississippi, Missouri, Ohio, and upper St. Lawrence can only be crossed at real bridges. That is why the rings stretch across open farmland, pinch at the Appalachians, and stall against the great rivers.",
    "Frodo never walked in a straight line, so the hero’s road wanders between the rings. It passes a real landmark wherever Frodo reached a milestone, and each leg is matched to the miles Frodo actually walked.",
    "Use the switch above the map to follow a second road, from the Hobbit Trail on the Oregon coast to the Hollywood Sign. From there, Frodo’s journey at its true length ends almost exactly on the sign."
  ],
  where: (name) => `Now at ${name}`,
  bagEndSteps: {
    1: { title: "Bag End", lede: "Arlington, Virginia",
         body: "The road begins at the Data Trust office. Every ring ahead is measured from this point, and the dashed line shows where the road will go." },
    2: { title: "Bag End", lede: "The Hobbit Trail, Oregon",
         body: "This road begins on the Hobbit Trail on the Oregon coast. Every ring ahead is measured from this point, and the dashed line shows where the road will go." }
  },
  ringLede: (m) => `Frodo had walked ${fmt(m.walkedMi)} miles by ${m.display}, yet he was only about ${fmt(m.displacementMi)} miles from Hobbiton as the Nazgûl flies.`,
  ringBody: (m) => `The darker ring on the map marks everywhere that much walking effort reaches from Bag End.`,
  ringPlaces: (names) => names.length ? `Notable places on or near it include ${listify(names)}.` : "",
  boatNote: "Frodo covered this leg by boat on the Anduin. Here it is walked.",
  routeReach: (r) => r.detour ? `The road passes ${r.detour} and reaches ${r.stop}.` : `The road reaches ${r.stop}.`,
  routeLeg: (r) => `This leg is about ${fmt(r.legMi)} miles of walking effort, against Frodo’s ${fmt(r.targetMi)}.`,
  endLine: (r) => `The road ends here after about ${fmt(r.totalMi)} miles of walking effort. Frodo walked about ${fmt(r.frodoMi)}.`,
  bookKicker: "In the book",
  // What happens to Frodo at each milestone (approved wording)
  bookEvents: {
    1: "Frodo, Sam, and Pippin escape a Black Rider by reaching the Bucklebury Ferry and crossing the Brandywine just ahead of it.",
    2: "At the Prancing Pony, the Ring slips onto Frodo’s finger and he vanishes in front of the crowd, and he meets Strider, who becomes the hobbits’ guide.",
    3: "On the summit of Weathertop, the Witch-king stabs Frodo with a Morgul blade, a wound that nearly turns him into a wraith.",
    4: "Elrond heals Frodo’s wound, and at the Council of Elrond, Frodo volunteers to carry the Ring to Mordor.",
    5: "Deep in the mines, Frodo’s mithril coat saves him from an orc spear, and Gandalf falls with the Balrog from the Bridge of Khazad-dûm.",
    6: "Galadriel shows Frodo her Mirror and gives him a phial of starlight that later saves his life in the dark.",
    7: "Boromir tries to take the Ring, and Frodo escapes by putting it on, then decides to go on to Mordor alone, though Sam refuses to be left behind.",
    8: "Gollum guides Frodo and Sam through the marshes, where pale faces of the long dead gleam beneath the water.",
    9: "Frodo watches the Witch-king lead his army out of the city, and the Ring pulls so hard on him that he nearly reveals himself.",
    10: "Gollum leads Frodo and Sam into Shelob’s lair, where Shelob stings Frodo, and Sam, believing him dead, takes the Ring.",
    11: "At the Cracks of Doom, Frodo cannot let go and claims the Ring, until Gollum bites it from his finger and falls with it into the fire."
  },
  // The real landmark on the North road at each milestone (approved wording)
  landmarkNotes: {
    1: {
      1: "In September 1862, Stonewall Jackson surrounded the Union garrison here and captured about 12,400 soldiers, the largest surrender of U.S. troops during the Civil War.",
      2: "The highest point on Blue Mountain, the eastern front range of Pennsylvania’s Ridge and Valley Appalachians, its summit ridge divides the waters flowing to the Susquehanna from those flowing to the Potomac.",
      3: "A 70 mile long ridge in Pennsylvania’s Allegheny Mountains, home to six state parks, with the Laurel Highlands Hiking Trail running its full length. A fitting hill for Weathertop.",
      // Draft, awaiting the author's approval
      4: "Hemlock-shaded gorges, waterfalls, and huge recess caves are carved into the Blackhand sandstone of southeastern Ohio here, and Ash Cave, 700 feet wide, is the largest recess cave in the state. A hidden valley of falling water, much like Rivendell.",
      6: "The highest peak in the Great Smoky Mountains and in Tennessee, Kuwohi had its Cherokee name restored in 2024 after more than a century as Clingmans Dome.",
      // Drafts below, awaiting the author's approval
      5: "The Linville River plunges about 90 feet in a series of falls into Linville Gorge, often called the Grand Canyon of the East, whose walls drop as much as 2,000 feet. A deep, echoing chasm fit for the road down into Moria.",
      7: "A reservoir on the Flint River in south Georgia, built from 1925 to 1930 when Crisp County became the first county in the nation to own and operate its own power plant. Georgia Veterans State Park, a memorial to the state’s veterans, sits on its eastern shore.",
      8: "North America’s largest blackwater swamp spreads across about 438,000 acres of southern Georgia and northern Florida, where floating islands of peat quiver underfoot. Its name is often translated as “land of the trembling earth,” a fitting stand-in for the Dead Marshes.",
      9: "One of Florida’s first-magnitude springs, pouring an average of about 100 million gallons of 72 degree water a day into the Suwannee River. In winter, West Indian manatees swim upriver to shelter in its warmth.",
      10: "Established in 1979 where the Suwannee River meets the Gulf of Mexico, this refuge protects one of the largest undeveloped river delta and estuary systems in the United States.",
      11: "Set aside by Theodore Roosevelt in 1908, it is among the oldest national forests east of the Mississippi and protects the world’s largest sand pine scrub. That scrub is made to burn: in 1935, a lightning fire there became the fastest spreading wildfire in Forest Service history, a fiery end for the quest."
    },
    2: {
      // Drafts, awaiting the author's approval
      1: "One of the larger lakes on the Oregon coast, formed behind sand dunes that dammed the mouths of old streams. Its name comes from a Siuslaw word meaning “having arms running out like a crab,” for its many branching coves.",
      2: "A Queen Anne house in Drain, Oregon, completed in 1895 for the youngest son of the town’s founder and known locally as the Drain Castle. Today it houses the local school district’s offices.",
      3: "A historic ranch at the mouth of Mule Creek, deep in the wild section of the Rogue River canyon and reachable by boat, trail, or a long backcountry road. Now a Bureau of Land Management museum, it tells the canyon’s story from gold mining to recreation.",
      4: "About 1.7 million acres of the Klamath Mountains along the California and Oregon border, set aside by Theodore Roosevelt in 1905, with 152 miles of wild and scenic rivers winding through deep canyons.",
      5: "A ghost town in a remote valley of northwestern Nevada, named for Vya Wimer, the first child of European descent born there; drought in the early 1920s ruined its farms. Decades later, a family snowbound on the empty road nearby survived after the father walked about 50 miles for help, a story later filmed as Snowbound.",
      6: "A vast, flat playa left behind by Lake Lahontan, an Ice Age lake, so smooth that Thrust SSC set the world land speed record here in 1997, the first car to break the sound barrier. Each summer it hosts Burning Man.",
      7: "Falling 2,425 feet in three tiers, it is the tallest waterfall in North America. Amon Hen stood beside the great Falls of Rauros, and this is a worthy stand-in.",
      8: "Once one of North America’s largest mercury mines, worked from 1854 until 1972, it left behind a ghost town and creeks poisoned with mercury, now an EPA Superfund site. Poisoned waters suit the Dead Marshes all too well.",
      9: "About 1.75 million acres of California’s coastal mountains, where the Sespe and Sisquoc sanctuaries shelter reintroduced California condors, among the rarest birds in the world.",
      10: "A valley of vineyards and ranches north of Santa Barbara, home to Mission Santa Inés, founded in 1804, and Solvang, a town founded by Danish Americans in 1911.",
      11: "Erected on Mount Lee in 1923 as a billboard reading HOLLYWOODLAND for a housing development, it lost its last four letters in 1949 and was rebuilt in steel in 1978. A mountain crowned with a famous name makes a fitting end for this road."
    }
  },
  roadName: { 1: "Arlington road", 2: "Hobbit Trail road" },
  yoursTitle: "Start your own journey",
  yoursIntro: "Choose a starting point on land, and the rings are recomputed in your browser with the same walking model: the same slopes, forests, wetlands, altitude, rivers, and bridges.",
  yoursReady: "Showing the rings from Bag End in Arlington. Search for a place above, or click anywhere on land, to move Bag End.",
  yoursWorking: (pct) => `Measuring walking effort from your Bag End… ${pct}%`,
  yoursDone: "Your rings are ready. Click anywhere else to start again.",
  yoursEdge: "Your rings are ready. Some reach the edge of the mapped area and are cut off there.",
  yoursWater: "That point is water or outside the mapped area. Choose a point on land.",
  yoursReset: "Return Bag End to Arlington",
  yoursPicking: "Choosing landmarks for your road…",
  yoursShake: "Find another road",
  searchPlaceholder: "Search for a place, or type latitude, longitude",
  originHeading: "Bag End",
  originLine: "Your journey begins here, at your own Bag End.",
  originClicked: "A point of your choosing",
  // --- Drafted by Claude, to be edited by the author: start ---
  loading: "Unrolling the map…",
  yoursLocate: "Use my location",
  locating: "Finding where you are…",
  locateDenied: "Location access is turned off for this page. Search for your town instead.",
  locateFailed: "Your location couldn’t be found. Search for your town instead.",
  originLocated: "Your location",
  searchFailed: "The list of places couldn’t be loaded. Try clicking the map instead.",
  stripElevation: "Elevation",
  stripLegs: "Legs",
  legsKeyRoad: (road) => `Walking effort on the ${road}`,
  legsKeyFrodo: "Frodo’s miles",
  legsLabel: (road) => `Each leg of the ${road} beside the same leg of Frodo’s journey, in miles`,
  legsTip: (r) => `<strong>${r.numeral} · ${r.name}</strong><br>${r.road}: ${r.roadMi} miles of walking effort<br>Frodo: ${r.frodoMi} miles`,
  legsTableHead: ["Leg", "Walking effort on this road (miles)", "Frodo’s miles"],
  jumpTo: (numeral, name) => `Go to ${numeral}, ${name}`,
  photoAlt: (name) => `Photo of ${name}, from its Wikipedia article`,
  photoBy: "Photo:",
  photoVia: ", via Wikimedia Commons",
  photoAuthorUnknown: "author unknown",
  // --- Drafted by Claude: end ---
  searchNone: "No matching places on this map",
  searchLandmark: "Landmark",
  searchCoords: "Coordinates",
  yoursShaken: "A new road from the same Bag End. Find another as often as you like.",
  yoursStatsTitle: "Your journey and Frodo’s",
  statYou: "Yours", statFrodo: "Frodo’s",
  statWalked: "Walking effort",
  statFromHome: "Distance from home at the end, as the Nazgûl flies",
  statWander: "Miles walked for every mile from home",
  statLongest: "Longest leg",
  statLandmarks: "Landmarks passed",
  statEnd: "Journey’s end",
  yoursRoadTitle: "Your road",
  yoursRoadIntro: "An estimated hero’s road from your Bag End, passing one real landmark at every milestone. Its legs are matched to Frodo’s miles with quick estimates, so treat the distances as approximate.",
  yoursStopLine: (r) => `${r.detour ? `Passing ${r.detour} on the way, your` : "Your"} road has covered about ${fmt(r.walkedMi)} miles of walking effort by here, yet you are only ${fmt(r.fromHomeMi)} miles from home as the Nazgûl flies. Frodo had walked ${fmt(r.frodoMi)}.`,
  yoursRoadShort: (name) => `Your road ends at ${name}, because the rings beyond it reach places this map has no landmarks for.`,
  notesTitle: "How this map was made",
  // How It Was Made: drafted text, to be edited by the author.
  // A paragraph of the form "[table]" inserts the land cover friction table.
  methodSections: [
    { title: "The question", paragraphs: [
      "What if Bag End stood in Arlington, Virginia? This map takes the distances of Frodo’s journey in The Lord of the Rings and lays them across North America, starting from the Data Trust office at 1310 North Courthouse Road. Instead of drawing circles of straight line distance, it asks how far a walker could actually get, because a mile of Ohio farmland and a mile of Appalachian ridges are not the same mile."
    ] },
    { title: "Measuring Frodo’s journey", paragraphs: [
      "The length of each leg comes from Karen Wynn Fonstad’s The Atlas of Middle-earth, the standard reference for distances in Tolkien’s world, as summarized by the Lord of the Steps project. In total, Frodo covers about 1,780 miles between Bag End and Mount Doom. The stretch from Lothlórien to Amon Hen was traveled by boat down the Anduin, but it is counted here as if it were walked.",
      "Distance walked is not the same as distance from home. Frodo spent hundreds of miles walking south along the Misty Mountains and down the Anduin while getting only a little farther from the Shire. So each ring is drawn at Frodo’s straight line distance from Hobbiton at that milestone, while the hero’s road between the rings is matched to the miles he actually walked. The straight line distances are estimates read from the 100 mile grid on Tolkien’s map and may be off by 10 to 15 percent."
    ] },
    { title: "The walking model", paragraphs: [
      "Walking speed follows Tobler’s hiking function, a formula fitted to real hikers that gives about 3.1 miles per hour on flat ground and slows sharply on steep grades. Because the map measures how hard a place is to reach in either direction, uphill and downhill pace are averaged. Slopes are computed from elevation at 250 meters, fine enough that steep ridges stay steep instead of being smoothed into gentle ramps. Elevation comes from AWS Terrain Tiles.",
      "Land cover adds friction. Each of the 19 classes in the 2010 North American Land Change Monitoring System, mapped at 250 meters, multiplies walking time by a factor, from 1.0 in towns to 2.5 in wetlands. These factors are judgment calls in the spirit of military terrain coefficients rather than measured values.",
      "[table]",
      "Thin air slows walkers too. Above 1,500 meters, about 4,900 feet, walking time rises 10 percent for every additional 1,000 meters of elevation.",
      "Water is a barrier. Coastlines and lakes come from Natural Earth. Four great rivers, the Mississippi, Missouri, Ohio, and upper St. Lawrence, can only be crossed at 40 bridge cities, each snapped to its river and opened as a gap 8 kilometers wide. Natural Earth’s line for the St. Lawrence ends upstream of Montreal, so the lower river is not treated as a barrier."
    ] },
    { title: "Drawing the rings", paragraphs: [
      "The walking model runs on a grid covering most of North America in an equal area projection, and each story map is drawn in an azimuthal equidistant projection centered on its Bag End, so distances outward from home look true. Walking pace is calculated on the 250 meter grid and then averaged up to a 5 kilometer grid. Pace is averaged rather than speed because travel times add up, and averaging speeds would understate how much a short, steep stretch slows a walker. A 5 kilometer cell counts as walkable if at least half of it is land.",
      "Walking effort from Bag End to every cell is then found with a cost distance calculation. Moving between grid cells allows only eight directions, which overcharges travel between those directions by up to 8 percent and turns every ring into an octagon, so each cell is corrected by the exact size of that error for its direction from home. A ring is drawn wherever the walking time equals Frodo’s straight line distance from Hobbiton divided by flat walking speed. On flat ground, the ring sits at exactly that distance, and every hill, forest, and river pulls it inward.",
      "As a check, walking from Arlington to Indianapolis costs about 1.22 times as much as walking the same straight line on flat ground, a plausible toll for crossing the Appalachians and their forests."
    ] },
    { title: "Finding the hero’s road", paragraphs: [
      "Landmarks come from Wikidata, which records coordinates for battlefields, waterfalls, peaks, bridges, observatories, and much more. Noteworthiness is measured by how many language editions of Wikipedia have an article about a place. Towns are kept only as a last resort. Things that are not really places, like languages, organizations, and sports teams, are removed, as are large areas pinned to a single point, like states, rivers, and mountain ranges. Sites of tragedy are excluded.",
      "A picker then chooses one landmark on every ring. It treats the rings as layers and searches for the chain whose legs best match the miles Frodo walked, with a bonus for fame, a penalty for doubling back, and a penalty for straight lines that cross water or a river barrier. A leg may pass one extra landmark on the way, which lets the road wander where Frodo walked far without getting much farther from home. Candidates are spread evenly around each ring, so the road can set out in any direction.",
      "The picker works from fast estimates, calibrated against true results. Every leg of the chosen road is then rebuilt from an exact cost distance surface and traced downhill along it. The picker’s suggestions were then reviewed one stop at a time, with some swapped by hand, and both roads were rebuilt around the final choices. The Arlington road’s leg misses add up to about 8 percent of Frodo’s distance, and its total comes within 1 percent of his 1,780 miles. The Hobbit Trail road’s misses add up to about 10 percent, and its total comes within 1 percent as well."
    ] },
    { title: "Your own journey", paragraphs: [
      "The Your Journey tab reruns the same model in your browser. The 5 kilometer pace grid, with slopes, land cover, altitude, rivers, and bridges already built in, is stored in the page as one byte per cell, and a shortest path search finds the walking effort from any point you choose. The rings are then traced from the result. The same picker then chooses an estimated hero’s road through real landmarks, using quick estimates in place of exact legs, and each landmark links to its Wikipedia article.",
      "The grid covers the contiguous United States, southern Canada up to 60 degrees north, Mexico, and Central America as far south as Guatemala, built with the same walking model on a Lambert azimuthal equal area projection centered on North America. Rings from starting points near its edges are cut off there."
    ] },
    { title: "Map design choices", paragraphs: [
      "The map is drawn like an old atlas: aged paper, inked coastlines, dashed borders, and mountains drawn as small peaks wherever the land rises at least 400 meters within 20 kilometers, thinned so they never crowd. Bridges use the traditional pair of brackets. Ring colors run from Shire straw to Mordor umber, darkening as the hero nears Mount Doom, and the map pulls back at each milestone to keep the current ring and the road so far in view."
    ] },
    { title: "Limitations", paragraphs: [
      "This is a model of a modern walker, not a hobbit, and it leaves a lot out. Land cover dates from 2010. Roads, trails, private land, seasons, and weather are all ignored. The 5 kilometer grid blurs small features, so a narrow river or gorge can disappear. The straight line distances from Hobbiton are approximate, and Wikidata’s coordinates and categories are sometimes wrong, so a landmark can occasionally be mislabeled or slightly out of place."
    ] },
    { title: "Photo credits", paragraphs: [
      "Landmark photos come from Wikimedia Commons and are shown with a sepia filter. Each is credited below with its author and license.",
      "[photoCredits]",
      // Drafted by Claude, to be edited by the author
      "Photos on the Your Journey tab are fetched live from Wikipedia when your road is chosen. Each is the lead image of the landmark’s English Wikipedia article, shown only if it is freely licensed, and is credited beneath it with its author and license."
    ] },
    { title: "Sources and tools", paragraphs: [
      "Frodo’s distances follow Karen Wynn Fonstad’s The Atlas of Middle-earth, as summarized by Lord of the Steps. Elevation comes from AWS Terrain Tiles; land cover from the Commission for Environmental Cooperation’s North American Land Change Monitoring System; coastlines, lakes, rivers, and borders from Natural Earth; and landmarks from Wikidata.",
      "The analysis was built in R with the terra, sf, slippymath, and rnaturalearth packages, and the page is drawn with D3. This project was built with help from Claude, an AI assistant made by Anthropic.",
      "Landmark descriptions on the Your Journey tab are the first sentences of English Wikipedia articles, used under the Creative Commons Attribution ShareAlike 4.0 license. Town and city names for the search box come from GeoNames, used under the Creative Commons Attribution 4.0 license."
    ] }
  ],
  frictionTable: [
    ["Urban", "1.0"], ["Cropland, grassland", "1.1"], ["Barren land", "1.2"],
    ["Shrubland, polar grass and barren lichen and moss", "1.3"],
    ["Temperate broadleaf deciduous forest, polar shrubs", "1.4"],
    ["Temperate needleleaf and mixed forest", "1.5"],
    ["Taiga, tropical broadleaf deciduous forest", "1.6"],
    ["Tropical broadleaf evergreen forest, snow and ice", "2.0"], ["Wetland", "2.5"], ["Water", "Impassable"]
  ],
  notes: [
    "Frodo’s leg distances follow Karen Wynn Fonstad’s The Atlas of Middle-earth, as summarized by Lord of the Steps. Straight-line distances from Hobbiton are rough estimates read from Tolkien’s map grid, so treat them as approximate.",
    "Walking speed follows Tobler’s hiking function on 250 meter terrain, with penalties for land cover and altitude, summed on a 5 kilometer grid. Elevation comes from AWS Terrain Tiles, land cover from the Commission for Environmental Cooperation’s North American Land Change Monitoring System, coastlines, lakes, rivers, and borders from Natural Earth, and landmarks from Wikidata."
  ]
};


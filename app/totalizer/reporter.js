const countryCodes = [
  "ALB",
  "ARM",
  "AUS",
  "AUT",
  "AZE",
  "BLR",
  "BEL",
  "BGR",
  "HRV",
  "CYP",
  "CZE",
  "DNK",
  "EST",
  "FIN",
  "FRA",
  "GEO",
  "DEU",
  "GRC",
  "ISL",
  "ISR",
  "IRL",
  "ITA",
  "LVA",
  "LTU",
  "MLT",
  "MDA",
  "NLD",
  "MKD",
  "NOR",
  "POL",
  "PRT",
  "ROU",
  "RUS",
  "SMR",
  "SRB",
  "SVN",
  "ESP",
  "SWE",
  "UKR",
  "GBR",
  "CHE",
];

const countryNames = [
  "Albania",
  "Armenia",
  "Australia",
  "Austria",
  "Azerbaijan",
  "Belarus",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czechia",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Georgia",
  "Germany",
  "Greece",
  "Iceland",
  "Israel",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Malta",
  "Moldova",
  "The Netherlands",
  "North Macedonia",
  "Norway",
  "Poland",
  "Portugal",
  "Romania",
  "Russia",
  "San Marino",
  "Serbia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Ukraine",
  "United Kingdom",
  "Switzerland",
];

const countryEmojis = [
  "&#127462;&#127473;",
  "&#127462;&#127474;",
  "&#127462;&#127482;",
  "&#127462;&#127481;",
  "&#127462;&#127487;",
  "&#127463;&#127486;",
  "&#127463;&#127466;",
  "&#127463;&#127468;",
  "&#127469;&#127479;",
  "&#127464;&#127486;",
  "&#127464;&#127487;",
  "&#127465;&#127472;",
  "&#127466;&#127466;",
  "&#127467;&#127470;",
  "&#127467;&#127479;",
  "&#127468;&#127466;",
  "&#127465;&#127466;",
  "&#127468;&#127479;",
  "&#127470;&#127480;",
  "&#127470;&#127473;",
  "&#127470;&#127466;",
  "&#127470;&#127481;",
  "&#127473;&#127483;",
  "&#127473;&#127481;",
  "&#127474;&#127481;",
  "&#127474;&#127465;",
  "&#127475;&#127473;",
  "&#127474;&#127472;",
  "&#127475;&#127476;",
  "&#127477;&#127473;",
  "&#127477;&#127481;",
  "&#127479;&#127476;",
  "&#127479;&#127482;",
  "&#127480;&#127474;",
  "&#127480;&#127472;",
  "&#127480;&#127473;",
  "&#127466;&#127480;",
  "&#127480;&#127466;",
  "&#127482;&#127462;",
  "&#127468;&#127463;",
  "&#127464;&#127469;",
];

const elems = {};
const scoreElems = {};

const scoreMap = {};
const targetScoreMap = {};

const setupScores = () => {
  const root = document.getElementById("scores");

  countryCodes.forEach((code) => {
    scoreMap[code] = 0;

    const ix = countryCodes.indexOf(code);

    const newElem = document.createElement("div");
    newElem.classList.add("score");
    newElem.id = code;

    const emoji = document.createElement("span");
    emoji.classList.add("emoji");
    emoji.innerHTML = countryEmojis[ix];
    newElem.appendChild(emoji);

    const countryName = document.createElement("span");
    countryName.classList.add("countryName");
    countryName.innerText = countryNames[ix];
    newElem.appendChild(countryName);

    const score = document.createElement("span");
    score.innerText = scoreMap[code];
    score.classList.add("value");
    newElem.appendChild(score);

    root.appendChild(newElem);

    scoreElems[code] = score;
    elems[code] = newElem;
  });

  twemoji.parse(document.body);
};

const floatScores = async () => {
  Object.entries(scoreMap).forEach(([code, val]) => {
    const targetVal = targetScoreMap[code];
    if (!targetVal) return;
    if (targetVal == val) return;

    elems[code].classList.add("moving");

    const diff = targetVal - val;
    if (diff < 1) {
      val = targetVal;
      elems[code].classList.remove("moving");
    } else {
      val = val + diff / 40;
    }
    scoreMap[code] = val;
    scoreElems[code].innerText = "" + Math.round(val);
  });

  // Reorder entries
  const codes = countryCodes.slice();
  codes.sort((a, b) => scoreMap[b] - scoreMap[a]);

  codes.forEach((v, i) => {
    elems[v].style.order = "" + i;
  });
};

const updateScores = async () => {
  // clear the report
  // get the new report
  const vals = await (await fetch("totals.json")).json();
  Object.assign(targetScoreMap, vals);
};

window.onload = () => {
  setupScores();
  setInterval(updateScores, 1000); // 1 tick per second
  setInterval(floatScores, 20); // 50 ticks per second
};

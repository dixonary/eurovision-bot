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
];

const runReporter = async () => {
  // clear the report
  const report = document.getElementById("report");

  const newReport = document.createElement("div");

  // get the new report
  const vals = await (await fetch("report.json")).json();

  vals.forEach((user) => {
    // user = {name, scores}
    const newElem = document.createElement("div");
    newElem.classList.add("report");
    const name = document.createElement("p");
    name.innerText = user.name;
    name.classList.add("name");
    newElem.appendChild(name);

    const scores = document.createElement("div");
    scores.classList.add("scores");
    newElem.appendChild(scores);

    [12, 10, 8, 7, 6, 5, 4, 3, 2, 1].forEach((val) => {
      const line = document.createElement("p");
      line.classList.add("line");

      const score = document.createElement("span");
      score.classList.add("score");
      score.innerText = val;

      const country = document.createElement("span");
      country.classList.add("country");
      country.innerText = user.scores[val] ?? "-";

      line.appendChild(score);
      line.appendChild(country);

      scores.appendChild(line);
    });

    newReport.appendChild(newElem);
  });

  report.innerHTML = newReport.innerHTML;
};
window.onload = () => {
  runReporter();
  setInterval(runReporter, 1000);
};

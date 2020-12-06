function drawChart(arr, indexes) {
    const ctx = document.getElementById("сhart");
    const data = {
        labels: indexes,
        datasets: [{
            label: "Численность популяций",
            borderColor: "rgba(75, 192, 192, 1)",
            data: arr, // сюда
            fill: false
        }]
    };
    const myBarChart = new Chart(ctx, {
        type: 'line',
        data: data,
        options: {
            scales: {
                yAxes: [{
                    ticks: {
                        beginAtZero: true
                    }
                }]
            }
        }
    });
}

let sliderIndividuals
let outputIndividuals
let sliderDeath
let outputDeath
let sliderIntraspecific
let outputIntraspecific
let sliderNumberOfChildren
let outputNumberOfChildren
let sliderTimeBetweenPregnancy
let outputTimeBetweenPregnancy
let sliderAveragePregnancyTime
let outputAveragePregnancyTime
let sliderMaxTime
let outputMaxTime

window.onload = function() {
    sliderIndividuals = document.getElementById("number-of-individuals");
    outputIndividuals = document.getElementById("number-of-individuals-text");
    sliderDeath = document.getElementById("death-coefficient");
    outputDeath = document.getElementById("death-coefficient-text");
    sliderIntraspecific = document.getElementById("intraspecific-competition");
    outputIntraspecific = document.getElementById("intraspecific-competition-text");
    sliderNumberOfChildren = document.getElementById("number-of-children");
    outputNumberOfChildren = document.getElementById("number-of-children-text");
    sliderTimeBetweenPregnancy = document.getElementById("time-between-pregnancy");
    outputTimeBetweenPregnancy = document.getElementById("time-between-pregnancy-text");
    sliderAveragePregnancyTime = document.getElementById("average-pregnancy-time");
    outputAveragePregnancyTime = document.getElementById("average-pregnancy-time-text");
    sliderMaxTime = document.getElementById("max-time");
    outputMaxTime = document.getElementById("max-time-text");
    generate();
    // здесь вызвать
    sliders();
}

function parametersInnerHTML() {
    outputIndividuals.innerHTML = sliderIndividuals.value;
    outputDeath.innerHTML = sliderDeath.value;
    outputIntraspecific.innerHTML = sliderIntraspecific.value;
    outputNumberOfChildren.innerHTML = sliderNumberOfChildren.value;
    outputTimeBetweenPregnancy.innerHTML = sliderTimeBetweenPregnancy.value;
    outputAveragePregnancyTime.innerHTML = sliderAveragePregnancyTime.value;
    outputMaxTime.innerHTML = sliderMaxTime.value;
}

function slider(slider, output) {
    slider.oninput = function() {
        output.innerHTML = this.value;
        generate();
    }
}

function sliders() {
    parametersInnerHTML();
    slider(sliderIndividuals, outputIndividuals);
    slider(sliderDeath, outputDeath);
    slider(sliderIntraspecific, outputIntraspecific);
    slider(sliderNumberOfChildren, outputNumberOfChildren);
    slider(sliderTimeBetweenPregnancy, outputTimeBetweenPregnancy);
    slider(sliderAveragePregnancyTime, outputAveragePregnancyTime);
    slider(sliderMaxTime, outputMaxTime);

    // sliderIndividuals.oninput = function() {
    //     outputIndividuals.innerHTML = this.value;
    //     generate()
    // }
    //
    // sliderDeath.oninput = function() {
    //     outputDeath.innerHTML = this.value;
    //     generate()
    // }
    //
    // sliderIntraspecific.oninput = function() {
    //     outputIntraspecific.innerHTML = this.value;
    //     generate()
    // }
    //
    // sliderNumberOfChildren.oninput = function() {
    //     outputNumberOfChildren.innerHTML = this.value;
    //     generate()
    // }
    //
    // sliderTimeBetweenPregnancy.oninput = function() {
    //     outputTimeBetweenPregnancy.innerHTML = this.value;
    //     generate()
    // }
    //
    // sliderAveragePregnancyTime.oninput = function() {
    //     outputAveragePregnancyTime.innerHTML = this.value;
    //     generate()
    // }
}

function generate(num = Number(sliderIndividuals.value),
                  deathCoeff = Number(sliderDeath.value),
                  numChildren = Number(sliderNumberOfChildren.value),
                  timeBetweenPreg = Number(sliderTimeBetweenPregnancy.value),
                  competitionCoeff = Number(sliderIntraspecific.value),
                  avePregTime = Number(sliderAveragePregnancyTime.value),
                  maxTime = Number(sliderMaxTime.value)) {
    var coords = new Array(maxTime + 1);
    coords[0] = num;

    for (let i = 1; i < maxTime + 1; ++i) {
        coords[i] = Math.max(coords[i - 1] +
            (coords[i - 1] * coords[i - 1] * numChildren * avePregTime) /
            (timeBetweenPreg + avePregTime * coords[i - 1]) -
            deathCoeff * coords[i - 1] -
            competitionCoeff * coords[i - 1] * coords[i - 1], 0);
    }

    var indexes = new Array(maxTime + 1)
    for (let i = 1; i <= maxTime; i++) {
        indexes[i] = i;
    }
    console.log(coords);
    document.getElementById("canvas-container").innerHTML = "<canvas id=\"сhart\"></canvas>";
    drawChart(coords, indexes)
}

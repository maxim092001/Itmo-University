function drawChart(arr, indexes) {
    const ctx = document.getElementById("сhart");
    const data = {
        labels: indexes,
        datasets: [{
            label: "Численность популяций",
            borderColor: "rgba(75, 192, 192, 1)",
            data: arr,
            fill: false
        },
         {
            label: "Размер популяции мышей", 
            borderColor: "rgb(229,11,11)",
            data: mouseData(),
            fill: false
        }
    ]
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
let parametersText

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
    parametersText = document.getElementById("parameters-text");
    parametersText.innerHTML = "<b>Размер популяции мышей.</b> <br> Количество особей в популяции: 4 <br> Коэффициент смертности: 0.5 <br> Внутривидовая конкуренция: 0.00151 <br> Количество детей: 2.8 <br> Время между беременностью в днях: 1 <br> Среднее время беременности в днях: 20"
    generate();
    sliders();
}

let mouseData = () => generateCoordinates(
        4,
        0.5,
        2.8,
        1,
    0.00151,
        20,
        sliderMaxTime === undefined ? 50 : Number(sliderMaxTime.value))

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
}

function generate(numberOfIndividuals = Number(sliderIndividuals.value),
                  deathCoefficient = Number(sliderDeath.value),
                  numberOfChildren = Number(sliderNumberOfChildren.value),
                  timeBetweenPregnancy = Number(sliderTimeBetweenPregnancy.value),
                  intraspecificCoefficient = Number(sliderIntraspecific.value),
                  averagePregnancyTime = Number(sliderAveragePregnancyTime.value),
                  maxTime = Number(sliderMaxTime.value)) {

    let coords = generateCoordinates(numberOfIndividuals,
        deathCoefficient,
        numberOfChildren,
        timeBetweenPregnancy,
        intraspecificCoefficient,
        averagePregnancyTime,
        maxTime)

    let indexes = new Array(maxTime + 1);
    for (let i = 1; i <= maxTime; i++) {
        indexes[i] = i;
    }

    document.getElementById("canvas-container").innerHTML = "<canvas id=\"сhart\"></canvas>";
    drawChart(coords, indexes)
}

function generateCoordinates(numberOfIndividuals,
                             deathCoefficient,
                             numberOfChildren,
                             timeBetweenPregnancy,
                             intraspecificCoefficient,
                             averagePregnancyTime,
                             maxTime) {
    let coords = new Array(maxTime + 1);
    coords[0] = numberOfIndividuals;

    for (let i = 1; i < maxTime + 1; i++) {
        coords[i] = Math.max(coords[i - 1] +
            (coords[i - 1] * coords[i - 1] * numberOfChildren * averagePregnancyTime) /
            (timeBetweenPregnancy + averagePregnancyTime * coords[i - 1]) -
            deathCoefficient * coords[i - 1] -
            intraspecificCoefficient * coords[i - 1] * coords[i - 1], 0);
    }
    return coords;
}

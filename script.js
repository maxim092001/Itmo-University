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
            data: [8, 26, 83, 262, 764, 1643, 1350, 1698, 1252, 1767, 1117, 1803, 1038, 1807, 1020, 1800, 1033, 1787, 1065, 1787, 1069, 1820, 1012, 1778, 1116, 1821, 982, 1797, 1067, 1785, 1056, 1803, 1071, 1819, 1038, 1821, 1037, 1784, 1093, 1788, 1044, 1775, 1064, 1802, 1030, 1737, 935, 1697, 1043, 1239, 821, 1103, 841, 1125, 807, 1079, 898, 1073, 853, 1107, 888, 1038, 1004, 836, 962, 817, 910, 865, 987, 823, 864, 869, 894, 746, 699, 801, 677, 810, 847, 867, 905, 728, 845, 843, 809, 722, 786, 678, 621, 590, 527, 509, 459, 398, 328, 289, 203, 170, 136, 101],
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
let bifurcationPoints

window.onload = function () {
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
    bifurcationPoints = document.getElementById("bifurcation-points");
    parametersText.innerHTML = "<b>Размер популяции мышей.</b> <br> Число особей в популяции: 4 <br> Коэффициент смертности: 0.5 <br> Внутривидовая конкуренция: 0.00151 <br> Среднее число детей: 2.8 <br> Время между беременностью в днях: 1 <br> Среднее время беременности в днях: 20"
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
    bifurcationPoints.innerHTML = generateBP();
}

function slider(slider, output) {
    slider.oninput = function () {
        output.innerHTML = this.value;
        bifurcationPoints.innerHTML = generateBP();
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

function generateBP(numberOfIndividuals = Number(sliderIndividuals.value),
                    deathCoefficient = Number(sliderDeath.value),
                    numberOfChildren = Number(sliderNumberOfChildren.value),
                    timeBetweenPregnancy = Number(sliderTimeBetweenPregnancy.value),
                    intraspecificCoefficient = Number(sliderIntraspecific.value),
                    averagePregnancyTime = Number(sliderAveragePregnancyTime.value)) {
    let x0 = (Math.sqrt(Math.pow(deathCoefficient * averagePregnancyTime + intraspecificCoefficient * timeBetweenPregnancy - numberOfChildren * averagePregnancyTime, 2)) - deathCoefficient * timeBetweenPregnancy) / (2 * intraspecificCoefficient * timeBetweenPregnancy)
    let x1 = (-Math.sqrt(Math.pow(deathCoefficient * averagePregnancyTime + intraspecificCoefficient * timeBetweenPregnancy - numberOfChildren * averagePregnancyTime, 2)) - deathCoefficient * timeBetweenPregnancy) / (2 * intraspecificCoefficient * timeBetweenPregnancy)
    return BP([x0, x1]);
}

function BP(arr) {
    let str = ""
    arr.filter(i => i > 0).forEach(function (value, i) {
        str = str + "x" + i + ": " + (value | 0) + "\n";
    });
    return "В данных точках производная равна 0: \n" + str;
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
            intraspecificCoefficient * coords[i - 1] * coords[i - 1], 0) | 0;
    }
    return coords;
}

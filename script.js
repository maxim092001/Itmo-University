function drawChart(individuals, infected) {
    const ctx = document.getElementById("myChart");
    const data = {
        labels: [1, 2, 3, 4, 5],
        datasets: [{
            label: "f(x) = x",
            function: function (x) {
                return x
            },
            borderColor: "rgba(75, 192, 192, 1)",
            data: [],
            fill: false
        },
            {
                label: "f(x) = x?",
                function: function (x) {
                    return x * x * infected
                },
                borderColor: "rgba(153, 102, 255, 1)",
                data: [],
                fill: false
            },
            {
                label: "f(x) = x * log(x)",
                function: function (x) {
                    return individuals * Math.log(x)
                },
                borderColor: "rgba(255, 206, 86, 1)",
                data: [],
                fill: false
            }]
    };
    Chart.plugins.register({
        beforeInit: function (chart) {
            const data = chart.config.data;
            for (let i = 0; i < data.datasets.length; i++) {
                for (let j = 0; j < data.labels.length; j++) {
                    const fct = data.datasets[i].function,
                        x = data.labels[j],
                        y = fct(x);
                    data.datasets[i].data.push(y);
                }
            }
        }
    });
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
let sliderInfected
let outputInfected

window.onload = function() {
    sliderIndividuals = document.getElementById("number-of-individuals");
    outputIndividuals = document.getElementById("number-of-individuals-text");
    sliderInfected = document.getElementById("number-of-infected");
    outputInfected = document.getElementById("number-of-infected-text");
    drawChart(sliderIndividuals.value, sliderInfected.value)
    sliders()
}

function sliders() {
    outputIndividuals.innerHTML = sliderIndividuals.value;
    outputInfected.innerHTML = sliderInfected.value
    sliderIndividuals.oninput = function() {
        outputIndividuals.innerHTML = this.value;
        drawChart(this.value, sliderInfected.value);
    }

    sliderInfected.oninput = function() {
        outputInfected.innerHTML = this.value;
        drawChart(sliderIndividuals.value, this.value)
    }
}

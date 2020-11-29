
function drawChart (individuals, infected) {
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
                    return x * x
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

function updateChart() {
    const individuals = getSliderValue("number-of-individuals");
    const infected = getSliderValue("number-of-infected");
    drawChart(individuals, infected);
}

window.onload = function() {
    drawChart(1, 1);
    slider("number-of-individuals", "number-of-individuals-text");
    slider("number-of-infected", "number-of-infected-text");
}

function slider(sliderId, outputId) {
    let slider = document.getElementById(sliderId);
    let output = document.getElementById(outputId);
    output.innerHTML = slider.value;
    slider.oninput = function() {
        output.innerHTML = this.value;
    }
}

function getSliderValue(sliderId) {
    return Number(document.getElementById(sliderId).value);
}

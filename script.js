
function drawChart (n, a) {
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
                    return a * Math.log(a)
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
    var n = Number(document.getElementById("num").value);
    var a = Number(document.getElementById("ill").value);
    console.log(n)
    drawChart(n, a)
}

window.onload = function() {
    drawChart(1, 1);
}
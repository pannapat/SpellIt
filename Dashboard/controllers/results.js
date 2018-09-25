$(document).ready(function () {

    function ajaxCallsFunc(type, url, contentType, data, callback) {
        $.ajax({
            type: type,
            url: url,
            contentType: contentType,
            data: data,
            success: callback
        });
    }



    window.onload = function () {
        console.log('loading')

        ajaxCallsFunc('POST', "http://127.0.0.1:5000/get_algos", 'application/json', null, function (branches) {
            var ab = new Object();
            for (var x in branches) {
                ab[branches[x]] = null;
            }

            addChips(ab);
        });


        ajaxCallsFunc('POST', "http://127.0.0.1:5000/get_results", 'application/json', null, function (branches) {


            var data = branches;

            var target_vars = []

            for (var x in data) {
                target_vars.push(x);
            }
            var algo_names = []

            for (var x in data[target_vars[0]]) {
                algo_names.push(x);
            }

            var metric_names = []
            var temp = target_vars[0];

            var temp2 = algo_names[0];
            var metric_pf = data[temp][temp2];

            for (var x in JSON.parse(metric_pf)) {
                metric_names.push(x);
            }

            var tab_count = 1;
            // console.log(target_vars[0]);
            for (var i = 0; i < target_vars.length; i++) {
                var target_var = target_vars[i];
                var data_dict = [];

                for (var j = 0; j < metric_names.length; j++) {
                    var metric_name = metric_names[j];
                    var algo_perf = {};
                    var x = []
                    var y = []

                    for (var k = 0; k < algo_names.length; k++) {
                        var algo_name = algo_names[k];
                        var t = data[target_var];
                        var a = t[algo_name];
                        var metric_vals = data[target_var][algo_name]
                        metric_perf = JSON.parse(metric_vals)[metric_name];
                        algo_perf[algo_name] = metric_perf;
                        x.push(algo_name);
                        y.push(metric_perf);
                    }
                    data_dict.push({ 'x': x, 'y': y, 'name': metric_name, 'type': 'bar' });

                }

                $plot_title = $('#plots-title');
                var tab = "<li class=\"tab col \"><a href=\"#plot" + tab_count.toString() + "\">" + target_var + "</a></li>";
                $plot_title.append(tab);

                $plot_content = $('#plots-content');
                var tab_content = "<div id=\"plot" + tab_count.toString() + "\" ></div>"
                $plot_content.append(tab_content);

                var tab_content_div = document.getElementById("plot" + tab_count.toString());
                

                var layout = {
                    title: 'Algorithm Metric Comparison of: "'+target_var+'"',
                    barmode: 'group',
                    bargap: 0.25,
                    bargroupgap: 0.1,
                    // barnorm: 'percent'
                };
                var barDiv = $('#bar-chart');
                Plotly.newPlot(tab_content_div, data_dict, layout);
                tab_count = tab_count + 1;
                $('.tabs').tabs();

            }




        });

    };

    function addChips(li) {
        console.log(li)
        $('#target_chips').chips({
            autocompleteOptions: {
                data: li,
                limit: Infinity,
                minLength: 1
            }
            
        });
        $("#target_chips").trigger('updateData');
    }

    $('#target_chips').on('chip.select', function(e, chip){
        console.log("Select",chip);
      });

});
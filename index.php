<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<link rel="icon" type="image/x-icon" href="../forecast.png">
	<title></title>
	<style type="text/css">
		html {
			padding: 50px;
			background-color: rgba(0, 0, 0, 1.0);
		}
		body {
			padding: 30px;
			background-color: white;
		}
	</style>

</head>
<?php 



function scan_dir($dir) {
    $ignored = array('.', '..', '.svn', '.htaccess');
    $files = array();

    foreach (scandir($dir) as $file) {
        if (in_array($file, $ignored)) continue;
        $files[$file] = filemtime($dir . '/' . $file); // Use filectime() for creation date
    }

    arsort($files); // Sort files by modification date in descending order
    $files = array_keys($files);
    return $files;
}



 ?>
<body class="bg-gray-100 flex items-center justify-center h-screen">

<table style="width: 100%;">
<tr>
	<td>
		<a href="../">< Home page</a>
	</td>
	<td>
		<a href="./export_csv.php" target="_blank">Download CSV Dataset</a>
	</td>
	<td>
		

<a href="https://www.hawaiitourismauthority.org/research/historical-visitor-statistics/">
	Hawaii Tourism Authority - Statistics Historical Data
</a>

	</td>
</tr>
</table>

	<table>
		<tr>
			<td></td>
		</tr>
	</table>

<style type="text/css">

	div, .container-fluid, .main-container {
		width: 100% !important;
		max-width: 100% !important;
	}
/*
	#header, .title {
		display: none  !important;
	}*/

	.math, .display {
		width: 100%;
		font-size: 1.2em;
	}

	p {
	    display: flex;
	    flex-wrap: wrap;
	    text-align: center;
	}

	p {
	    margin: 5px; /* Optional: Adds some space between paragraphs */
	}
</style>


<?php 	
require_once 'latex.html';
// require_once 'math_symbol.html';
?>





<style>
    .container {
        display: flex;
        flex-wrap: wrap;
    }
    .image {
        flex: 1 1 auto;
        max-width: 100%;
        margin: 5px;
    }
    ._3_image {
    	width: 33.33%;
    }
    ._4_image {
    	width: 24%;
    }
    ._5_image {
    	width: 19%;
    }
    canvas {
        width: 100%;
    }
</style>


<h1>
  Analytical Calculus-based Mathematical-Statistic Tactical Data-Driven Decision Making System
</h1>

<hr>


  <div class="bg-white p-6 rounded-lg shadow-lg space-y-4">





    




    
    <a href="?">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Home
      </button>
    </a>

    <a href="?whole-interpolation">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Whole Interpolation
      </button>
    </a>

    <a href="?single-period-of-seasonality">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Each single period of seasonality
      </button>
    </a>

    <a href="?iteration-forecasting">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Iteration Forecasting
      </button>
    </a>


    <a href="?error-iteration-forecasting">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Error Iteration Forecasting
      </button>
    </a>







    <a href="?forecasting-semantic-model-annually">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Psychological Activity Reinforcement - Cubic Spline Interpolation
      </button>
    </a>
    <a href="?hta-ticket-sales-correlation">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        HTA and Ticket Sales Correlation - Seasonal
      </button>
    </a>
  	<a href="?cubic-spline-daily-interpolation">
  		<button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
  			Cubic Spline Annually Interpolation
  		</button>
  	</a>
  	<a href="?lagrange-annually-interpolation">
  		<button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
  			Lagrange Annually Interpolation & Richardson Extrapolation
  		</button>
  	</a>
  	<a href="?software-architecture">
  		<button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
  			Software Architecture
  		</button>
  	</a>



    <a href="?source-code">
      <button class="w-full px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition">
        Source Code
      </button>
    </a>











  </div>











<script type="text/javascript" src="https://cdn.jsdelivr.net/npm/chart.js"></script>


<?php 

// Database connection
global $host; $host = 'localhost';
global $db; $db = 'pcc_forecast_db';
global $user; $user = 'pcc-sdr';
global $pass; $pass = '6zu_.Ldx2.zCNb_8';

 ?>


<?php 
$is_home = (sizeof($_GET)==0);
 ?>


<?php 
if (sizeof($_GET)==0) {
?>



<?php 
}
 ?>






























<?php 

function draw_xy_graph($title,$query)
{
  
  global $host;
  global $db;
  global $user;
  global $pass;

 ?>

<?php 
$mysqli = new mysqli($host, $user, $pass, $db);
if ($mysqli->connect_error) {
    die('Connection failed: ' . $mysqli->connect_error);
}
 ?>

<?php

// Fetch data
// $query = "SELECT x, y FROM temp_forecast_daily ORDER BY  x ASC";
$result = $mysqli->query($query);

$data = [];
while ($row = $result->fetch_assoc()) {
    $data[] = $row;
}

?>


<?php 

$mysqli->close();

 ?>



    <h1>
      <?php echo $title; ?>
    </h1>
    <canvas id="forecastChart <?php echo $title; ?>" width="500" height="400"></canvas>

    <script>
        var chartData = <?php echo json_encode($data); ?>;

        var labels = chartData.map(item => item.x);
        var values = chartData.map(item => item.y);

        var ctx = document.getElementById('forecastChart <?php echo $title; ?>').getContext('2d');
        var forecastChart = new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: [{
                    label: '<?php echo $title; ?>',
                    data: values,
                    borderColor: 'rgba(75, 192, 192, 1)',
                    backgroundColor: 'rgba(75, 192, 192, 0.2)',
                    fill: true,
                    tension: 0.3
                }]
            },
            options: {
                responsive: true,
                scales: {
                    x: {
                        title: {
                            display: true,
                            text: 'X'
                        }
                    },
                    y: {
                        title: {
                            display: true,
                            text: 'Y'
                        }
                    }
                }
            }
        });
    </script>

<?php 
}


 ?>
















<?php 

function draw_xy_graph_big_data($title, $query)
{
    global $host;
    global $db;
    global $user;
    global $pass;

    $mysqli = new mysqli($host, $user, $pass, $db);
    if ($mysqli->connect_error) {
        die('Connection failed: ' . $mysqli->connect_error);
    }

    // Fetch data
    $result = $mysqli->query($query);

    $data = [];
    while ($row = $result->fetch_assoc()) {
        $data[] = $row;
    }

    $mysqli->close();
?>

    <h1>
      <?php echo $title; ?>
    </h1>
    <canvas id="forecastChart<?php echo preg_replace('/[^A-Za-z0-9]/', '', $title); ?>" width="500" height="400"></canvas>

    <script>
        var chartData = <?php echo json_encode($data); ?>;
        var chunkSize = 10; // Number of points to add per interval
        var intervalTime = 100; // Time between updates in milliseconds
        var currentIndex = 0;

        var labels = [];
        var values = [];
        var ctx = document.getElementById('forecastChart<?php echo preg_replace('/[^A-Za-z0-9]/', '', $title); ?>').getContext('2d');
        var forecastChart = new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: [{
                    label: '<?php echo $title; ?>',
                    data: values,
                    borderColor: 'rgba(75, 192, 192, 1)',
                    backgroundColor: 'rgba(75, 192, 192, 0.2)',
                    fill: true,
                    tension: 0.3
                }]
            },
            options: {
                responsive: true,
                scales: {
                    x: {
                        title: {
                            display: true,
                            text: 'X'
                        }
                    },
                    y: {
                        title: {
                            display: true,
                            text: 'Y'
                        }
                    }
                }
            }
        });

        // Function to update chart with next chunk of data
        function updateChart() {
            if (currentIndex < chartData.length) {
                var nextChunk = chartData.slice(currentIndex, currentIndex + chunkSize);
                nextChunk.forEach(item => {
                    labels.push(item.x);
                    values.push(item.y);
                });
                currentIndex += chunkSize;

                forecastChart.data.labels = labels;
                forecastChart.data.datasets[0].data = values;
                forecastChart.update();
            } else {
                clearInterval(chartInterval); // Stop when all data is displayed
            }
        }

        // Start the interval to update chart
        var chartInterval = setInterval(updateChart, intervalTime);
    </script>

<?php 
}
?>























<?php 

function draw_xy_graph_2($title,$query,$title_data_1,$title_data_2)
{
    global $host;
    global $db;
    global $user;
    global $pass;
     ?>

    <?php 
    $mysqli = new mysqli($host, $user, $pass, $db);
    if ($mysqli->connect_error) {
        die('Connection failed: ' . $mysqli->connect_error);
    }
     ?>

    <?php
    // Fetch data
    // $query = "SELECT x, y FROM temp_forecast_daily ORDER BY  x ASC";
    $result = $mysqli->query($query);
    $data = [];
    while ($row = $result->fetch_assoc()) {
        $data[] = $row;
    }
    ?>

    <?php 
    $mysqli->close();
     ?>

    <h1>
      <?php echo $title; ?>
    </h1>
    <canvas id="forecastChart <?php echo $title; ?>" width="500" height="400"></canvas>

    <script>
        var chartData = <?php echo json_encode($data); ?>;


// chartData.map(item => {
//   console.log(typeof item.y, typeof item.y2, item.y, item.y2);
//   return Math.abs(item.y - item.y2);
// });


        var labels = chartData.map(item => item.x);
        var values = chartData.map(item => item.y);
        var values2 = chartData.map(item => item.y2);
        var values3 = chartData
  .map(item => ((item.y == null || item.y2 == null)? null: Math.abs(item.y - item.y2)))
  ;



        var ctx = document.getElementById('forecastChart <?php echo $title; ?>').getContext('2d');
        var forecastChart = new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: [
                {
                  label: '<?php echo $title_data_1; ?>',
                  data: values,
                  borderColor: 'rgba(75, 192, 192, 1)',
                  backgroundColor: 'rgba(75, 192, 192, 0.2)',
                  fill: true,
                  tension: 0.3,
                  spanGaps: true
                },
                {
                  label: '<?php echo $title_data_2; ?>',
                  data: values2,
                  borderColor: 'rgba(192, 192, 75, 1)',
                  backgroundColor: 'rgba(192, 192, 75, 0.2)',
                  fill: true,
                  tension: 0.3,
                  spanGaps: true
                },
                {
                  label: 'Error',
                  data: values3,
                  borderColor: 'rgba(75, 192, 0, 1)',
                  backgroundColor: 'rgba(75, 192, 0, 0.2)',
                  fill: true,
                  tension: 0.3,
                  spanGaps: true
                }
              ]
            },
            options: {
                responsive: true,
                scales: {
                    x: {
                        title: {
                            display: true,
                            text: 'X'
                        }
                    },
                    y: {
                        title: {
                            display: true,
                            text: 'Y'
                        }
                    }
                }
            }
        });
    </script>

<?php 
}


 ?>














<?php 
date_default_timezone_set('Pacific/Honolulu');
// Helper function to convert normalized year to DateTime
function dateFromNormalized($x) {
    $year = floor($x);
    $fraction = $x - $year;
    $daysInYear = date("L", strtotime("$year-01-01")) ? 366 : 365;
    // echo "x $x \n";
    // echo "daysInYear $daysInYear \n";
    // echo "fraction $fraction \n";
    $dayOfYear = round($fraction * $daysInYear);
    // echo "dayOfYear $dayOfYear \n";

// $year = 2023;
// $dayOfYear = 100; // example: 100th day of the year

$date = DateTime::createFromFormat('Y z', "$year " . $dayOfYear);
$formatted = $date->format('Y-m-d H:i:s');

// echo "formatted date: $formatted\n";

    // echo "formatted date:  $formatted \n ";

    return $formatted; // z is 0-based
}

// Helper function to convert DateTime to normalized year
function normalizedYear($date) {
    $year = (int)$date->format('Y');
    $dayOfYear = (int)$date->format('z') + 1;
    $daysInYear = $date->format('L') ? 366 : 365;
    return $year + ($dayOfYear / $daysInYear);
}


 ?>









<?php   
if (isset($_GET['whole-interpolation'])) {
 ?>
<table style="width: 50%;">
    <tr>
        <td>
            
<?php 
    draw_xy_graph_big_data(
        "Entire PAX Historical Interpolation",
        "SELECT x, y FROM temp_forecast_daily ORDER BY  x ASC"
    );


?>
        </td>
    </tr>
</table>
<?php   
}
 ?>

<?php   
if (isset($_GET['single-period-of-seasonality'])) {
 ?>
<style type="text/css">
    td {
        vertical-align: text-top;
    }
</style>
<table style="width: 100%;">
    <tr>













        <td style="width: 33%;">
<?php 

?>
            <table>
<?php 

$mysqli = new mysqli($host, $user, $pass, $db);
if ($mysqli->connect_error) {
    die('Connection failed: ' . $mysqli->connect_error);
}
$result = $mysqli->query("SELECT MIN(x) AS min_x, MAX(x) AS max_x FROM temp_forecast_daily;");
$mysqli->close();

while ($row = $result->fetch_assoc()) {
    // Convert to DateTime
    $firstDate = new DateTime(dateFromNormalized($row['min_x']));
    $firstDate->modify('first day of January');

    $lastDate = new DateTime(dateFromNormalized($row['max_x']));
    $lastDate->modify('last day of December');
}

// Loop through each year
$currentYear = clone $firstDate;
while ($currentYear <= $lastDate) {
?>
    <tr>
        <td>
<?php
    $yearEnd = clone $currentYear;
    $yearEnd->modify('last day of December');

    $xStart = normalizedYear($currentYear);
    $xEnd = normalizedYear($yearEnd);
    $title = "Year " . $currentYear->format('Y') . " (" . $currentYear->format('Y-m-d') . " to " . $yearEnd->format('Y-m-d') . ")";

    draw_xy_graph(
        "$title",
        "SELECT x, y FROM temp_forecast_daily WHERE x >= $xStart AND x <= $xEnd ORDER BY x ASC"
    );
    echo "\n";

    $currentYear->modify('first day of January next year');
?>
        </td>
    </tr>
<?php
}
 ?>
            </table>
        </td>


































        <td style="width: 33%;">
<?php 

?>
            <table>
<?php 


$mysqli = new mysqli($host, $user, $pass, $db);
if ($mysqli->connect_error) {
    die('Connection failed: ' . $mysqli->connect_error);
}
$result = $mysqli->query("SELECT MIN(x) AS min_x, MAX(x) AS max_x FROM temp_forecast_daily;");
$mysqli->close();

while ($row = $result->fetch_assoc()) {
    // Convert to DateTime
    $firstDate = new DateTime(dateFromNormalized($row['min_x']));
    $firstDate->modify('first day of this month');

    $lastDate = new DateTime(dateFromNormalized($row['max_x']));
    $lastDate->modify('last day of this month');
}

// Loop through each month
$currentMonth = clone $firstDate;
while ($currentMonth <= $lastDate) {
?>
    <tr>
        <td>
<?php
    $monthEnd = clone $currentMonth;
    $monthEnd->modify('last day of this month');

    $xStart = normalizedYear($currentMonth);
    $xEnd = normalizedYear($monthEnd);
    $title = "Month of " . $currentMonth->format('F Y') . " (" . $currentMonth->format('Y-m-d') . " to " . $monthEnd->format('Y-m-d') . ")";

    draw_xy_graph(
        "$title",
        "SELECT x, y FROM temp_forecast_daily WHERE x >= $xStart AND x <= $xEnd ORDER BY x ASC"
    );
    echo "\n";

    $currentMonth->modify('first day of next month');
?>
        </td>
    </tr>
<?php
}
 ?>
            </table>
        </td>




























        <td style="width: 33%;">
<?php 

?>
            <table>
<?php 


    $mysqli = new mysqli($host, $user, $pass, $db);
    if ($mysqli->connect_error) {
        die('Connection failed: ' . $mysqli->connect_error);
    }
    $result = $mysqli->query("SELECT MIN(x) AS min_x, MAX(x) AS max_x FROM temp_forecast_daily;");
    $mysqli->close();
    while ($row = $result->fetch_assoc()) {
        // var_dump($row);
        // Convert to DateTime
        $firstDate = dateFromNormalized($row['min_x']);
        $lastDate = dateFromNormalized($row['max_x']);
        // Adjust to first Monday
        $firstDate=(new DateTime($firstDate));
        if ($firstDate->format('N') != 1) {
            $firstDate->modify('next Monday');
        }
        // Adjust to last Monday
        $lastDate=(new DateTime($lastDate));
        if ($lastDate->format('N') != 1) {
            $lastDate->modify('last Monday');
        }
    }

    // Loop through each Monday
    $currentMonday = clone $firstDate;
    while ($currentMonday <= $lastDate) {
?>
                <tr>
                    <td>
<?php 
        $sunday = clone $currentMonday;
        $sunday->modify('+6 days');
        $xStart = normalizedYear($currentMonday);
        $xEnd = normalizedYear($sunday);
        $title = "Week of " . $currentMonday->format('Y-m-d') . " to " . $sunday->format('Y-m-d') . "";
        draw_xy_graph(
            "$title",
            "SELECT x, y FROM temp_forecast_daily WHERE x >= $xStart AND x <= $xEnd ORDER BY x ASC"
        );
        echo "\n";
        $currentMonday->modify('+7 days');
?>
                    </td>
                </tr>
<?php 
    }
 ?>
            </table>
        </td>




















    </tr>
</table>

<?php 
}
 ?>



<?php 

function date_denormalize($normalized_date)
{
  global $host;
  global $db;
  global $user;
  global $pass;

  $mysqli = new mysqli($host, $user, $pass, $db);
  if ($mysqli->connect_error) {
      die('Connection failed: ' . $mysqli->connect_error);
  }
  $sql = "CALL `denormalize`(".$normalized_date.");";
  // echo $sql;
  $result_ = $mysqli->query($sql);
  while ($row_ = $result_->fetch_assoc()) {
    $normalized_origin = $row_['normalized_origin'];
  }
  $mysqli->close();

  return $normalized_origin;
}

 ?>





<?php 
if (isset($_GET['iteration-forecasting']) || $is_home) {
   ?>
  <h1>
    Ticket Sales Forecasting and Prediction
  </h1>
  <hr>
  <?php 
  $mysqli = new mysqli($host, $user, $pass, $db);
  if ($mysqli->connect_error) {
      die('Connection failed: ' . $mysqli->connect_error);
  }

  $sql_ordering = "";
  $query = "
    SELECT
      `start-point`
      , count(*) c 
    FROM `forecast_result`
    group by `start-point` 
    ORDER BY 
    SUM((
        SELECT `temp_forecast_daily`.`y` 
        FROM `temp_forecast_daily` 
        WHERE `temp_forecast_daily`.`x`=`forecast_result`.`end-point`
        limit 1
      )) DESC,
    AVG(ABS((
      `forecast_result`.`value`-(
        SELECT `temp_forecast_daily`.`y` 
        FROM `temp_forecast_daily` 
        WHERE `temp_forecast_daily`.`x`=`forecast_result`.`end-point`
        limit 1
      )
    ))) ASC
    limit 50;
  ";
  $query = "
    SELECT
      `start-point`
      , count(*) c 
    FROM `forecast_result`
    group by `start-point` 
    ORDER BY `start-point` DESC
    limit 50;
  ";
  $result = $mysqli->query($query);
  $mysqli->close();

  ?>
  <style type="text/css">
    td {
      padding: 10px;
      margin: 10px;
    }
  </style>
  <table>
    <tr>
  <?php 
  $loop_ind = 0;
  while ($row = $result->fetch_assoc()) {

    ?>
    <td>

    <?php

    draw_xy_graph_2(
      "Iterative Forecasting ".date_denormalize($row['start-point']),
      "
        SELECT 
        `end-point` `x`, 
        `value` `y`, 
        (
          SELECT `temp_forecast_daily`.`y` 
          FROM `temp_forecast_daily` 
          WHERE `temp_forecast_daily`.`x`=`forecast_result`.`end-point`
          limit 1
        ) `y2`
        FROM `forecast_result`
        WHERE `start-point`='{$row['start-point']}'
        ORDER BY `end-point` ASC
        limit 90
        ;
      ",
      "Forecasting",
      "Historic"
    );

    // draw_xy_graph_2(
    //   "Iterative Forecasting ".date_denormalize($row['start-point']),
    //   "
    //     SELECT 
    //     `end-point` `x`, 
    //     `value` `y`, 
    //     (
    //       SELECT `temp_forecast_daily`.`y` 
    //       FROM `temp_forecast_daily` 
    //       WHERE `temp_forecast_daily`.`x`=`forecast_result`.`end-point`
    //       limit 1
    //     ) `y2`
    //     FROM `forecast_result`
    //     WHERE `start-point`='{$row['start-point']}'
    //     ORDER BY `end-point` ASC
    //     limit 90
    //     ;
    //   ",
    //   "Forecasting",
    //   "Historic"
    // );

      ?>
     <h5>

     Forecasting data-points: <?php 
    echo $row['c'];
     ?>
<br>
    Starting Date -> <?php 
      echo $row['start-point'];
     ?> - <?php 
    echo $row['start-point'];
     ?> 
     </h5>

      </td>
      <?php 

    $loop_ind += 1;
    if ($loop_ind == 3)  {
      $loop_ind = 0;
      ?>
      </tr>
      <tr>
      <?php 
    }
  }
  ?>
    </tr>
  </table>
  <?php 
}
 ?>


















<?php 
if ( isset($_GET['error-iteration-forecasting']) ) {
   ?>
  <hr>
  <table style="height: 600px;">
    <tr>
      <td style="width: 1000px;">
  <h1>
    Error Calculating with incremental duration
  </h1>
  <hr>
    <?php
    draw_xy_graph(
      "Error Iterative Forecasting with incremental duration",
      "CALL `select_forecast_error_incremental_forecast_duration`();"
    );

   ?>
      </td>
      <td style="width: 800px;" hidden>
        
  <h1>
    Error Calculating
  </h1>
  <hr>
    <?php
    draw_xy_graph(
      "Error Iterative Forecasting ",
      "CALL `select_forecast_error`();"
    );
}
 ?>

      </td>
    </tr>
  </table>
  <hr>















<?php 





 ?>









<style type="text/css">
	td {
		text-align: left;
	}
</style>











<?php 

if (isset($_GET['forecasting-semantic-model-annually'])) {
 ?>
<h2>Psychological Activity Reinforcement - Cubic Spline Interpolation & Richardson Extrapolation</h2>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Color Meanings for Ticket Sales Analysis</title>
    <style>
        body {
            margin: 0 auto;
            padding: 20px;
            line-height: 1.6;
        }
        h1 {
            text-align: center;
            color: #333;
        }
        ul {
            list-style-type: none;
            padding: 0;
        }
        li {
            margin: 15px 0;
            padding: 10px;
            border-left: 5px solid;
        }
        .black { border-color: black; background-color: #f0f0f0; }
        .purple { border-color: purple; background-color: #f3e5f5; }
        .red { border-color: red; background-color: #ffebee; }
        .orange { border-color: orange; background-color: #fff3e0; }
        .green { border-color: green; background-color: #e8f5e9; }
        .color-box {
            display: inline-block;
            width: 20px;
            height: 20px;
            vertical-align: middle;
            margin-right: 10px;
        }
    </style>
</head>
<body>
    <h4>Color Meanings in "Nex Pax" Ticket Sales Analysis</h4>
    <ul>
        <li class="black">
            <span class="color-box" style="background-color: black;"></span>
            <strong>Black Curve</strong>: Represents the interpolation curve of the raw daily ticket sales data for "Nex Pax."
        </li>
        <li class="purple">
            <span class="color-box" style="background-color: purple;"></span>
            <strong>Purple Curve</strong>: Represents the first derivative, indicating the speed of ticket sales changes each day.
        </li>
        <li class="red">
            <span class="color-box" style="background-color: red;"></span>
            <strong>Red Curve</strong>: Represents the second derivative, indicating the force pushing ticket sales upward or downward.
        </li>
        <li class="orange">
            <span class="color-box" style="background-color: orange;"></span>
            <strong>Orange Curve</strong>: Represents the third derivative, reflecting the speed of executing decisions from administration.
        </li>
        <li class="green">
            <span class="color-box" style="background-color: green;"></span>
            <strong>Green Curve</strong>: Represents the fourth derivative, indicating the speed of decision-making processes, capturing the periodic routine of organizational and psychological activities of tourists or management, analogous to someone pushing an object based on their decisions.
        </li>
    </ul>
    <p><em>Note</em>: The first three derivatives (black, purple, red) are designed to capture features of ticket sales number variations, while the last two derivatives (orange, green) analyze the periodic routines of organizational and psychological activities related to tourists or management.</p>
</body>
</html>










<?php
function draw_multiple_xy_graphs($title, $curve_data) {
?>
    <h1><?php echo $title; ?></h1>
    <canvas id="forecastChart_<?php echo md5($title); ?>" width="600" height="400"></canvas>

    <script>
        var allCurveData = <?php echo json_encode($curve_data); ?>;

        var labels = [...new Set(Object.values(allCurveData).flat().map(item => item.x))].sort((a, b) => a - b);

        var colors = [
            'rgba(255, 99, 132, 1)',
            'rgba(54, 162, 235, 1)',
            'rgba(255, 206, 86, 1)',
            'rgba(75, 192, 192, 1)',
            'rgba(153, 102, 255, 1)',
            'rgba(255, 159, 64, 1)'
        ];

        var datasets = Object.entries(allCurveData).map(([name, data], index) => {
            var dataMap = Object.fromEntries(data.map(point => [point.x, point.y]));
            var values = labels.map(x => dataMap[x] ?? null);

            return {
                label: name,
                data: values,
                borderColor: colors[index % colors.length],
                backgroundColor: colors[index % colors.length].replace('1)', '0.2)'),
                fill: false,
                tension: 0.3
            };
        });

        var ctx = document.getElementById('forecastChart_<?php echo md5($title); ?>').getContext('2d');
        new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: datasets
            },
            options: {
                responsive: true,
                scales: {
                    x: {
                        title: {
                            display: true,
                            text: 'X'
                        }
                    },
                    y: {
                        title: {
                            display: true,
                            text: 'Y'
                        }
                    }
                }
            }
        });
    </script>
<?php
}
?>








<?php
function draw_multiple_xy_graphs_by_two_sql($graph_name, $name_list_sql, $curve_points_sql)
{
    global $host;
    global $db;
    global $user;
    global $pass;

    $curve_names = [];

    $mysqli = new mysqli($host, $user, $pass, $db);
    if ($mysqli->connect_error) {
        die('Connection failed: ' . $mysqli->connect_error);
    }
    $result = $mysqli->query($name_list_sql);
    // "SELECT `name` FROM `curve_points` GROUP BY `name`;"
    $mysqli->close();
    $column_index = 0;
    while ($row = $result->fetch_assoc()) {
        // var_dump($row);
        $curve_name = $row['name'];
        array_push($curve_names, $curve_name);
    }

    $curve_data = [];

    $mysqli = new mysqli($host, $user, $pass, $db);
    if ($mysqli->connect_error) {
        die('Connection failed: ' . $mysqli->connect_error);
    }

    foreach ($curve_names as $name) {
        $stmt = $mysqli->prepare($curve_points_sql);
        // "SELECT `x`, `y` FROM `curve_points` WHERE `name` = ?"
        $stmt->bind_param("s", $name);
        $stmt->execute();
        $result = $stmt->get_result();

        $points = [];
        while ($row = $result->fetch_assoc()) {
            $points[] = $row;
        }

        $curve_data[$name] = $points;
        $stmt->close();
    }


    $newKeys = array_map(function($key) {
        return str_replace('./forecasting-semantic-model-annually/', '', $key);
    }, array_keys($curve_data));
    $curve_data = array_combine($newKeys, array_values($curve_data));




    $mysqli->close();
    // "Multi-Curve Comparison"
    draw_multiple_xy_graphs($graph_name, $curve_data);

}
?>


<table>
    <tr>
        <td>
<?php 
draw_multiple_xy_graphs_by_two_sql(

    "Average Curve Annually", 

    "SELECT `name` 
    FROM `curve_points` 
    WHERE 1=1
        AND `name` LIKE '%avg%' 
        AND `name` LIKE '%annually-%' 
    GROUP BY `name`;", 

    "SELECT `x`, `y` 
    FROM `curve_points` 
    WHERE `name` = ?"

);
 ?>
        </td>
        <td>
<?php 
draw_multiple_xy_graphs_by_two_sql(

    "Average Curve Monlthly", 

    "SELECT `name` 
    FROM `curve_points` 
    WHERE 1=1
        AND `name` LIKE '%avg%' 
        AND `name` LIKE '%monthly%' 
    GROUP BY `name`;", 

    "SELECT `x`, `y` 
    FROM `curve_points` 
    WHERE `name` = ?"

);
 ?>
        </td>
        <td>
<?php 
draw_multiple_xy_graphs_by_two_sql(

    "Average Curve Weekly", 

    "SELECT `name` 
    FROM `curve_points` 
    WHERE 1=1
        AND `name` LIKE '%avg%' 
        AND `name` LIKE '%week%' 
    GROUP BY `name`;", 

    "SELECT `x`, `y` 
    FROM `curve_points` 
    WHERE `name` = ?"

);
 ?>
        </td>
    </tr>
    <tr>
        <td>
<?php 
draw_multiple_xy_graphs_by_two_sql(

    "STD Curve Annually", 

    "SELECT `name` 
    FROM `curve_points` 
    WHERE 1=1
        AND `name` LIKE '%std%' 
        AND `name` LIKE '%annually-%' 
    GROUP BY `name`;", 

    "SELECT `x`, `y` 
    FROM `curve_points` 
    WHERE `name` = ?"

);
 ?>
        </td>
        <td>
<?php 
draw_multiple_xy_graphs_by_two_sql(

    "STD Curve Monlthly", 

    "SELECT `name` 
    FROM `curve_points` 
    WHERE 1=1
        AND `name` LIKE '%std%' 
        AND `name` LIKE '%monthly%' 
    GROUP BY `name`;", 

    "SELECT `x`, `y` 
    FROM `curve_points` 
    WHERE `name` = ?"

);
 ?>
        </td>
        <td>
<?php 
draw_multiple_xy_graphs_by_two_sql(

    "STD Curve Weekly", 

    "SELECT `name` 
    FROM `curve_points` 
    WHERE 1=1
        AND `name` LIKE '%std%' 
        AND `name` LIKE '%week%' 
    GROUP BY `name`;", 

    "SELECT `x`, `y` 
    FROM `curve_points` 
    WHERE `name` = ?"

);
 ?>
        </td>
    </tr>
</table>





<div class="container">
<table>
    <tr>
            
<?php
    $mysqli = new mysqli($host, $user, $pass, $db);
    if ($mysqli->connect_error) {
        die('Connection failed: ' . $mysqli->connect_error);
    }
    $result = $mysqli->query("SELECT `name` FROM `curve_points` WHERE `name` LIKE '%-f1%' or `name` LIKE '%-s1%' GROUP BY `name`;");
    $mysqli->close();
    $column_index = 0;
    while ($row = $result->fetch_assoc()) {
        // var_dump($row);
        $average_curve_name = $row['name'];
        $curve_name = str_replace('forecasting-semantic-model-annually', '', $average_curve_name);
        $curve_name = str_replace('/', '', $curve_name);
        $curve_name = str_replace('.', '', $curve_name);
        ?>
        <td>
        <?php
        echo $curve_name.'<br>';
        draw_xy_graph(
            "".$curve_name,
            "SELECT `id`, `name`, `x`, `y` FROM `curve_points` WHERE `name`='$average_curve_name';"
        );
        ?>
        </td>
        <?php
        $column_index += 1;
        if ($column_index >=3) {
            ?>
            </tr>
            <tr>
            <?php
            $column_index = 0;
        }
    }
?>
    </tr>
</table>





<?php

$dirname = "./forecasting-semantic-model-annually/"; // Specify your folder path


$sorted_files = scan_dir($dirname);
// print_r($sorted_files);

// $images = glob($dirname . "*.png");
$images = $sorted_files;
foreach ($images as $image) {
  if (strpos($image, ".png")!==false) {
    echo '<img src="' . $dirname.$image . '" class="_4_image" />';
  }
}
?>
</div>

<?php 
}
 ?>





<?php 

if (isset($_GET['cubic-spline-daily-interpolation'])) {
 ?>
<h2>Cubic Spline Interpolation & Richardson Extrapolation</h2>

<div class="container">
<?php

$dirname = "./forecasting-cubicspline/"; // Specify your folder path


$sorted_files = scan_dir($dirname);
// print_r($sorted_files);

// $images = glob($dirname . "*.png");
$images = $sorted_files;
foreach ($images as $image) {
	if (strpos($image, ".png")!==false) {
    echo '<img src="' . $dirname.$image . '" class="_4_image" />';
	}
}
?>
</div>

<?php 
}
 ?>













<?php 

if (isset($_GET['lagrange-annually-interpolation'])) {

 ?>




<b>
	Equation:
</b>



<table style="width:100%;">
	<tr>
		<td>
<span class="math display">
\[
S(x) = \begin{cases} 
S_0(x) = a_0 + b_0(x - x_0) + c_0(x - x_0)^2 + d_0(x - x_0)^3 & \text{for } x_0 \leq x \leq x_1 \\
S_1(x) = a_1 + b_1(x - x_1) + c_1(x - x_1)^2 + d_1(x - x_1)^3 & \text{for } x_1 \leq x \leq x_2 \\
\vdots & \vdots \\
S_{n-1}(x) = a_{n-1} + b_{n-1}(x - x_{n-1}) + c_{n-1}(x - x_{n-1})^2 + d_{n-1}(x - x_{n-1})^3 & \text{for } x_{n-1} \leq x \leq x_n
\end{cases}
\]
</span>
		</td>
		<td>
			<table>
				<tr>
					<td>
<span class="math display">
\[
f(x_0+h)=f'(x_0)h+ \frac{f''(\xi)}{2} h^2 
\]
</span>
					</td>
				</tr>
				<tr>
					<td>
<span class="math display">
\[
f(x_0)=\frac{f(x_0+h)-f(x_0)}{h}-\frac{f''(\xi)}{2} h
\]
</span>
					</td>
				</tr>
			</table>
		</td>
	</tr>
</table>






























<p>

<h1>Lagrange Interpolation & Richardson Extrapolation</h1>

<div class="container">
<?php

$dirname = "./forecasting/"; // Specify your folder path

$sorted_files = scan_dir($dirname);
// print_r($sorted_files);

// $images = glob($dirname . "*.png");
$images = $sorted_files;
foreach ($images as $image) {
	if (strpos($image, ".png")!==false) {
    echo '<img src="' . $dirname.$image . '" class="_4_image" />';
	}
}
?>
</div>
















<b>Equation:</b>

<span class="math display">
\[
P_{1}\left(x,n,m\right)=\sum_{i=n}^{m}\left(y_{1}\left[i\right]\left(\prod_{j=n}^{i-1}\frac{\left(x-x_{1}\left[j\right]\right)}{x_{1}\left[i\right]-x_{1}\left[j\right]}\right)\left(\prod_{k=i+1}^{m}\frac{\left(x-x_{1}\left[k\right]\right)}{x_{1}\left[i\right]-x_{1}\left[k\right]}\right)\right)\left\{x_{1}\left[n\right]&lt;x&lt;x_{1}\left[m\right]\right\}
\]

\[
f(x_0+h)=f'(x_0)h+ \frac{f''(\xi)}{2} h^2 
\]

\[
f(x_0)=\frac{f(x_0+h)-f(x_0)}{h}-\frac{f''(\xi)}{2} h
\]
</span></p>









<h1>
	Interpolation and Extrapolation
</h1>
<iframe src="https://www.desmos.com/calculator/mnbcmjyanl" style="width: 100%;height: 800px;"></iframe>

<hr>
<hr>






















<hr>

<p>
<h2>Closed Newton-Cotes Formulas</h2>
<h3></h3>
<span class="math display">
\[
\begin{matrix}
\text{Trapezoid rule} 
& \text{Simpson's rule}
& \text{Simpson's three-eights rule} \\
\int_{x_0}^{x_1} f(x) dx
=\frac{h}{2} 
[y_0+y_1]
-\frac{h^3}{12} f''(\xi)
&
\int_{x_0}^{x_2} f(x) dx
=\frac{h}{3} 
[y_0+4 y_1 +y_2]
-\frac{h^5}{90} f^{(4)}(\xi)
&
\int_{x_0}^{x_3} f(x) dx
=\frac{3h}{8} 
[y_0+3 y_1 +3y_2+y_3]
-\frac{3h^5}{80} f^{(4)}(\xi) \\
Boole's rule & n=5 & n=6 \\
\int_{x_0}^{x_4} f(x) dx
=\frac{h}{2} 
[y_0+y_1]
-\frac{h^3}{12} f''(\xi)
&
\int_{x_0}^{x_5} f(x) dx
=\frac{h}{3} 
[y_0+4 y_1 +y_2]
-\frac{h^5}{90} f^{(4)}(\xi)
&
\int_{x_0}^{x_6} f(x) dx
=\frac{h}{3} 
[y_0+4 y_1 +y_2]
-\frac{h^5}{90} f^{(4)}(\xi)
\end{matrix}
\]
</span></p>









<?php 

}

 ?>























<?php 

if (isset($_GET['software-architecture'])) {

 ?>



<h1>Software Architecture</h1>

<a href="./Fully-Automated-Forecasting-Model.drawio">Fully-Automated-Forecasting-Model</a>

<img src="./Fully-Automated-Forecasting-Model.drawio.png" style="width: 100%;">




<?php 

}

 ?>

















<?php 

if (isset($_GET['hta-ticket-sales-correlation'])) {

 ?>




<h1>
	HTA and Ticket Sales Correlation
</h1>


<div class="container">
<?php
$dirname = "./correlation/"; // Specify your folder path
$images = glob($dirname . "*.png");

foreach ($images as $image) {
    echo '<img src="' . $image . '" class="_3_image" />';
}
?>
</div>

<hr>
<hr>
<hr>


<div class="container">
<?php
$dirname = "./data_visualization/"; // Specify your folder path
$images = glob($dirname . "*.png");

foreach ($images as $image) {
    echo '<img src="' . $image . '" class="_3_image" />';
}
?>
</div>




<?php 	
require_once './forecasting-cubicspline.html';

require_once './forecasting.html';
require_once './correlation.html';


 ?>


<script type="text/javascript">
// setTimeout(function () {
// 	document.querySelectorAll('pre').forEach(function(element) {
// 	    element.style.display = 'none';
// 	});

// },10);
</script>



<?php 

}

 ?>












<?php 
if (isset($_GET['source-code'])) {
 ?>
<?php
// Check if a file path is provided via GET parameter
$filePath = isset($_GET['file']) ? $_GET['file'] : './forecasting-cubicspline-daily.r';

// echo $filePath;

// Validate file path and ensure it has .R extension
if (!empty($filePath) && file_exists($filePath) && pathinfo($filePath, PATHINFO_EXTENSION) === 'r') {
    // Read the file contents
    $code = file_get_contents($filePath);
    // Escape HTML characters to safely display code
    $code = htmlspecialchars($code);
} else {
    $code = null;
}
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>R Source Code Viewer</title>
    <!-- Include Highlight.js CSS from CDN -->
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/default.min.css">
    <!-- Include Highlight.js JavaScript from CDN -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
    <!-- Include R language support for Highlight.js -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/r.min.js"></script>
    <script>
        // Initialize Highlight.js on page load
        document.addEventListener('DOMContentLoaded', (event) => {
            document.querySelectorAll('pre code').forEach((block) => {
                hljs.highlightElement(block);
            });
        });
    </script>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 20px;
        }
        pre {
            background-color: #f4f4f4;
            padding: 10px;
            border-radius: 5px;
        }
    </style>
</head>
<body>
    <?php if ($code !== null): ?>
        <h2>R Source Code: <?php echo htmlspecialchars(basename($filePath)); ?></h2>
        <pre><code class="language-r"><?php echo $code; ?></code></pre>
    <?php else: ?>
        <h2>Error</h2>
        <p>Please provide a valid .R file path using ?file=path/to/file.R in the URL.</p>
    <?php endif; ?>
</body>
</html>
<?php 
}
 ?>







<script>

// add bootstrap table styles to pandoc tables
function bootstrapStylePandocTables() {
  $('tr.odd').parent('tbody').parent('table').addClass('table table-condensed');
}
$(document).ready(function () {
  bootstrapStylePandocTables();
});


</script>

<!-- tabsets -->

<script>
$(document).ready(function () {
  window.buildTabsets("TOC");
});

$(document).ready(function () {
  $('.tabset-dropdown > .nav-tabs > li').click(function () {
    $(this).parent().toggleClass('nav-tabs-open');
  });
});
</script>

<!-- code folding -->


<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
  (function () {
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src  = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
    document.getElementsByTagName("head")[0].appendChild(script);
  })();
</script>





</body>
</html>
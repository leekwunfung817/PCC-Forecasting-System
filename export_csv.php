<?php


$servername = "localhost";
$username = "pcc-sdr";
$password = "6zu_.Ldx2.zCNb_8";
$dbname = "pcc_forecast_db";

// Create connection
$conn = new mysqli($servername, $username, $password, $dbname);

// Check connection
if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}
?>
<?php
$sql = "
DROP TABLE IF EXISTS `csv_export`;
";


$result = $conn->query($sql);
$sql = "
CREATE TABLE csv_export
with 
htaticketing_data as (
	SELECT
	    concat(
	        `Year`,
	        '-',
	        LPAD((
	            SELECT CASE `Month`
	                WHEN 'January' THEN 1
	                WHEN 'February' THEN 2
	                WHEN 'March' THEN 3
	                WHEN 'April' THEN 4
	                WHEN 'May' THEN 5
	                WHEN 'June' THEN 6
	                WHEN 'July' THEN 7
	                WHEN 'August' THEN 8
	                WHEN 'September' THEN 9
	                WHEN 'October' THEN 10
	                WHEN 'November' THEN 11
	                WHEN 'December' THEN 12
	                ELSE NULL
	            END AS month_number
	        ), 2, '0')
	    ) `YYYY-MM`,
	    `TicketSell`
	FROM
	    `htaticketing`
)
SELECT 
	`htaticketing_data`.`YYYY-MM`,
	'HTA All Visitors by air' `Data Source`,
	`Group`,
	`value`,
	`TicketSell` 
FROM `htaallvisitorsbyair`, `htaticketing_data`
WHERE `htaallvisitorsbyair`.`YYYY-MM`=`htaticketing_data`.`YYYY-MM`
union
SELECT 
	`htaticketing_data`.`YYYY-MM`,
	'HTA Accommodation Choices' `Data Source`,
	`Group`,
	`value`,
	`TicketSell` 
FROM `htaaccommodationchoices`, `htaticketing_data`
WHERE `htaaccommodationchoices`.`YYYY-MM`=`htaticketing_data`.`YYYY-MM`
union
SELECT 
	`htaticketing_data`.`YYYY-MM`,
	'HTA Method of Trave' `Data Source`,
	`Group`,
	`value`,
	`TicketSell` 
FROM `htamethoodoftrave`, `htaticketing_data`
WHERE `htamethoodoftrave`.`YYYY-MM`=`htaticketing_data`.`YYYY-MM`
union
SELECT 
	`htaticketing_data`.`YYYY-MM`,
	'HTA Purpose of Trip' `Data Source`,
	`Group`,
	`value`,
	`TicketSell` 
FROM `htapurposeoftrip`, `htaticketing_data`
WHERE `htapurposeoftrip`.`YYYY-MM`=`htaticketing_data`.`YYYY-MM`
union
SELECT 
	`htaticketing_data`.`YYYY-MM`,
	'Tenant Sales' `Data Source`,
	`Tenant Name`,
	`Net Sales`,
	`TicketSell` 
FROM `tenantsales`, `htaticketing_data`
WHERE DATE_FORMAT(`tenantsales`.`Date`, '%Y-%m')=`htaticketing_data`.`YYYY-MM`
;
";
$result = $conn->query($sql);

$sql = "
SELECT 
	`YYYY-MM`, 
	`Data Source`, 
	`Group`, 
	sum(`value`) `Dependent Value`, 
	sum(`TicketSell`) `TicketSell` 
FROM csv_export 
GROUP BY `YYYY-MM`, `Data Source`, `Group`;
";
$result = $conn->query($sql);

if ($result->num_rows > 0) {
    // Open output stream
    $output = fopen('php://output', 'w');

    // Set headers to force download
    header('Content-Type: text/csv');
    header('Content-Disposition: attachment; filename="correlation.data.csv"');

    // Fetch and write the column headers
    $fields = $result->fetch_fields();
    $headers = array();
    foreach ($fields as $field) {
        $headers[] = $field->name;
    }
    fputcsv($output, $headers);

    // Fetch and write the rows
    while ($row = $result->fetch_assoc()) {
        fputcsv($output, $row);
    }

    // Close output stream
    fclose($output);
} else {
    echo "No records found.";
}

// Close connection
$conn->close();
?>

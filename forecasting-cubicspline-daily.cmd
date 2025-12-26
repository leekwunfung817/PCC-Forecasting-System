

cd "C:/xampp/htdocs/DataProcessor/"
cd "./forecasting/"
python ./syn_net_pax.py
python ./holiday_generate.py

cd "C:\xampp\htdocs\TacticalDataDrivenDecisionSystem"
"C:/Program Files/R/R-4.4.3/bin/Rscript.exe" ./forecasting-cubicspline-daily.r
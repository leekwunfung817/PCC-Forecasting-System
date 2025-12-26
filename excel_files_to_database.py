import os
import pandas as pd
# openpyxl
# python -m pip install openpyxl
# python -m pip install mariadb

def scan_excel_files(folder_path):
    # List to store the paths of all Excel files
    excel_files = []

    # Iterate over all files in the folder
    for root, dirs, files in os.walk(folder_path):
        for file in files:
            # Check if the file is an Excel file
            if file.endswith('.xlsx') or file.endswith('.xls'):
                # Add the full path of the Excel file to the list
                excel_files.append(os.path.join(root, file))

    return excel_files





import mariadb
import sys

try:
    conn = mariadb.connect(
        user="pcc-sdr",
        password="6zu_.Ldx2.zCNb_8",
        host="localhost",
        port=3306,
        database="pcc_forecast_db"
    )
except mariadb.Error as e:
    print(f"Error connecting to MariaDB Platform: {e}")
    sys.exit(1)

# Get a cursor
cur = conn.cursor()





# Example usage
folder_path = "C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\data"  # Replace with the path to your folder
excel_files = scan_excel_files(folder_path)

# Read and print the content of each Excel file found
for excel_file in excel_files:
    file_path = excel_file
    # Determine the engine based on the file extension
    if file_path.endswith('.xlsx'):
        engine = 'openpyxl'
    elif file_path.endswith('.xls'):
        engine = 'xlrd'
    else:
        raise ValueError("Unsupported file format")


    if 'HTA Accommodation Choices.xlsx' in excel_file:
        # Read the Excel file
        df = pd.read_excel(file_path, engine=engine)

        # Melt the DataFrame to reorganize it
        df_melted = df.melt(id_vars=['Group', 'Units', 'Indicator'], var_name='YYYY-MM', value_name='value')

        # Generate and print SQL insert statements
        for index, row in df_melted.iterrows():
            sql = f"INSERT INTO `htaaccommodationchoices`(`Group`, `Units`, `Indicator`, `YYYY-MM`, `value`) VALUES ('{row['Group']}','{row['Units']}','{row['Indicator']}','{row['YYYY-MM']}','{row['value']}')"
            # print(sql)
            try:
                # Execute a query
                print(sql)
                cur.execute(sql)
                conn.commit()

                # Fetch the results
                # for row in cur:
                #     print(row)
            except mariadb.Error as e:
                print(f"Error: {e}")


    elif 'HTA Ticketing.xlsx' in excel_file:

        df = pd.read_excel(file_path, engine=engine, header=1)
        # Remove the Grand Total row
        df = df[df['Row Labels'] != 'Grand Total']

        # print(df)
        # Melt the dataframe to get Year, Month, and TicketSell columns
        df_melted = pd.melt(
            df,
            id_vars=['Row Labels'],
            var_name='Year',
            value_name='TicketSell'
        )

        # Rename 'Row Labels' to 'Month'
        df_melted = df_melted.rename(columns={'Row Labels': 'Month'})

        # print(df_melted)
        # Replace NaN with 0 and convert TicketSell to integer
        df_melted['TicketSell'] = df_melted['TicketSell'].fillna(0)

        # Generate SQL INSERT statements
        sql_template = "INSERT INTO `htaticketing`(`Year`, `Month`, `TicketSell`) VALUES ('{}','{}','{}')"

        for index, row in df_melted.iterrows():
            if 'Total PAX' == str(row['Month']) or 'Grand Total' == row['Year'] or "nan" in str(row['Month']) or "Row Labels" in str(row['Month']):
                continue
            sql_statement = sql_template.format(row['Year'], row['Month'], row['TicketSell'] )
            # print(sql_statement)
            try:
                # Execute a query
                print(sql)
                cur.execute(sql_statement)
                conn.commit()

                # Fetch the results
                # for row in cur:
                #     print(row)
            except mariadb.Error as e:
                print(f"Error: {e}")

        # Print the reorganized dataframe
        # print("\nReorganized DataFrame:")
        # print(df_melted)

    elif 'HTA All Visitors by Air.xlsx' in excel_file:
        # Read the Excel file
        df = pd.read_excel(file_path, engine=engine, header=1)
        # print(df)

        # Melt the DataFrame to reorganize it
        df_melted = df.melt(id_vars=['Group', 'Indicator', 'Units'], var_name='YYYY-MM', value_name='value')

        # Generate and print SQL insert statements
        for index, row in df_melted.iterrows():
            if 'nan' == str(row['Indicator']) or 'nan' == str(row['Units']):
                continue
            sql = f"INSERT INTO `htaallvisitorsbyair`(`Group`, `Indicator`, `Units`, `YYYY-MM`, `value`) VALUES ('{row['Group']}','{row['Indicator']}','{row['Units']}','{row['YYYY-MM']}','{row['value']}')"
            # print(sql)
            try:
                # Execute a query
                print(sql)
                cur.execute(sql)
                conn.commit()

                # Fetch the results
                # for row in cur:
                #     print(row)
            except mariadb.Error as e:
                print(f"Error: {e}")
    elif 'HTA Methood of Trave.xlsx' in excel_file:
        # Read the Excel file
        df = pd.read_excel(file_path, engine=engine, header=1)
        # print(df)

        # Melt the DataFrame to reorganize it
        df_melted = df.melt(id_vars=['Group', 'Indicator', 'Units'], var_name='YYYY-MM', value_name='value')

        # Generate and print SQL insert statements
        for index, row in df_melted.iterrows():
            if 'nan' == str(row['Indicator']) or 'nan' == str(row['Units']):
                continue
            sql = f"INSERT INTO `htamethoodoftrave`(`Group`, `Indicator`, `Units`, `YYYY-MM`, `value`) VALUES ('{row['Group']}','{row['Indicator']}','{row['Units']}','{row['YYYY-MM']}','{row['value']}')"
            # print(sql)
            try:
                # Execute a query
                print(sql)
                cur.execute(sql)
                conn.commit()

                # Fetch the results
                # for row in cur:
                #     print(row)
            except mariadb.Error as e:
                print(f"Error: {e}")
    elif 'HTA Purpose of Trip.xlsx' in excel_file:
        # Read the Excel file
        df = pd.read_excel(file_path, engine=engine, header=1)
        # print(df)

        # Melt the DataFrame to reorganize it
        df_melted = df.melt(id_vars=['Group', 'Indicator', 'Units'], var_name='YYYY-MM', value_name='value')

        # Generate and print SQL insert statements
        for index, row in df_melted.iterrows():
            if 'nan' == str(row['Indicator']) or 'nan' == str(row['Units']):
                continue
            sql = f"INSERT INTO `htapurposeoftrip`(`Group`, `Indicator`, `Units`, `YYYY-MM`, `value`) VALUES ('{row['Group']}','{row['Indicator']}','{row['Units']}','{row['YYYY-MM']}','{row['value']}')"
            # print(sql)
            try:
                # Execute a query
                print(sql)
                cur.execute(sql)
                conn.commit()

                # Fetch the results
                # for row in cur:
                #     print(row)
            except mariadb.Error as e:
                print(f"Error: {e}")
    elif 'Tenant Sales.xlsx' in excel_file:
        # Read the Excel file
        df = pd.read_excel(file_path, engine=engine, header=0)
        print(df)





        # Assuming your dataframe is called 'df'
        # First, let's create a list to store our reorganized data
        reorganized_data = []

        # Define the column pattern (each set has 3 columns: Date, Tenant Name, Net Sales)
        base_columns = ['Date', 'Tenant Name', 'Net Sales']

        # Function to process each set of columns
        def process_column_set(df, prefix=''):
            date_col = prefix + 'Date' if prefix else 'Date'
            tenant_col = prefix + 'Tenant Name' if prefix else 'Tenant Name'
            sales_col = prefix + 'Net Sales' if prefix else 'Net Sales'
            
            # Extract the three columns and rename them consistently
            temp_df = df[[date_col, tenant_col, sales_col]].copy()
            temp_df.columns = ['Date', 'TenantName', 'NetSales']
            return temp_df

        # Process original columns
        reorganized_data.append(process_column_set(df))

        # Process all duplicated columns (they follow pattern Date.#, Tenant Name.#, Net Sales.#)
        max_sets = 30  # Based on your sample going up to .30
        for i in range(1, max_sets + 1):
            prefix = f'{i}.'
            try:
                # Check if these columns exist
                if f'Date.{i}' in df.columns:
                    reorganized_data.append(process_column_set(df, prefix))
            except:
                break

        # Combine all dataframes
        final_df = pd.concat(reorganized_data, ignore_index=True)

        # Generate SQL statements
        sql_template = "INSERT INTO `tenantsales`(`Date`, `Tenant Name`, `Net Sales`) VALUES ('{}','{}',{})"

        for index, row in final_df.iterrows():
            # Handle '(Blank)' values by converting to NULL or empty string as needed
            date = row['Date']
            tenant = row['TenantName']
            sales = row['NetSales'] if row['NetSales'] != '(Blank)' else 'NULL'
            
            # Format the SQL statement
            sql = sql_template.format(date, tenant, sales)
            # print(sql)
            try:
                # Execute a query
                print(sql)
                cur.execute(sql)
                conn.commit()

                # Fetch the results
                # for row in cur:
                #     print(row)
            except mariadb.Error as e:
                print(f"Error: {e}")

        # # Melt the DataFrame to reorganize it
        # df_melted = df.melt(id_vars=['Date', 'Date Ratio'], var_name='Store', value_name='value')

        # # Generate and print SQL insert statements
        # for index, row in df_melted.iterrows():
        #     if 'nan' == str(row['Indicator']) or 'nan' == str(row['Units']):
        #         continue
        #     sql = f"INSERT INTO `tenantsales`(`Date`, `Tenant Name`, `Net Sales`) VALUES ('{row['Date']}','{row['Tenant Name']}','{row['Net Sales']}')"
        #     # print(sql)
    else:
        # Print the content of the Excel file
        # print(f"Content of {file_path}:")
        # print(df)
        print("\n")




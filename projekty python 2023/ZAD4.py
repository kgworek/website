import requests
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Funkcja do pobierania informacji o kursach walut/złota
def get_nbp_data(table, start_date, end_date):
    url = f'https://api.nbp.pl/api/{table}/{start_date}/{end_date}/?format=json'
    response = requests.get(url)

    if response.status_code == 200:
        return response.json()
    else:
        print(f"Problem z uzyskaniem danych: {response.status_code}")
        if response.status_code == 400:
            print("Problem z uzyskaniem danych, sprawdź wprowadzaone parametry.")
        return None

# Funkcja do przetwarzania danych i tworzenia tabeli DataFrame dla cen złota
def process_gold_data(start_date, end_date):
    data_dict = {'Date': [], 'Price': []}

    current_date = start_date
    while current_date <= end_date:
        fragment_end_date = current_date + timedelta(days=92)
        if fragment_end_date > end_date:
            fragment_end_date = end_date

        fragment_data = get_nbp_data('cenyzlota', current_date, fragment_end_date)
        for single_date in pd.date_range(current_date, fragment_end_date):
            effective_date_str = single_date.strftime('%Y-%m-%d')

            if fragment_data:
                gold_info = next((item for item in fragment_data if item.get('data') == effective_date_str), None)
                if gold_info:
                    data_dict['Date'].append(effective_date_str)
                    data_dict['Price'].append(gold_info['cena'])
                else:
                    data_dict['Date'].append(effective_date_str)
                    data_dict['Price'].append(None)
            else:
                data_dict['Date'].append(effective_date_str)
                data_dict['Price'].append(None)

        current_date = fragment_end_date + timedelta(days=1)

    # Tworzenie DataFrame z danymi
    df_gold = pd.DataFrame(data_dict)

    return df_gold

# Funkcja do przetwarzania danych  dla kursów walut oraz dodawania NaN dla weekendów
def process_currency_data(start_date, end_date):
    currency_data_dict = {'Date': []}

    current_date = start_date
    while current_date <= end_date:
        fragment_end_date = current_date + timedelta(days=92)
        if fragment_end_date > end_date:
            fragment_end_date = end_date

        fragment_data = get_nbp_data('exchangerates/tables/A', current_date, fragment_end_date)
        for single_date in pd.date_range(current_date, fragment_end_date):
            effective_date_str = single_date.strftime('%Y-%m-%d')

            currency_data_dict['Date'].append(effective_date_str)

            rates_for_date = {}
            if fragment_data:
                for rates_info in fragment_data:
                    effective_date = rates_info.get('effectiveDate', None)
                    if effective_date == effective_date_str:
                        rates_for_date.update({rate['code']: rate['mid'] for rate in rates_info['rates']})

            # Obsługa braku danych
            if not rates_for_date:
                for rate_code in currency_data_dict.keys():
                    if rate_code != 'Date':
                        currency_data_dict[rate_code].append(np.nan)
            else:
                # Aktualizacja słownika poza pętlą
                for rate_code in rates_for_date.keys():
                    if rate_code not in currency_data_dict:
                        currency_data_dict[rate_code] = []
                    currency_data_dict[rate_code].append(rates_for_date[rate_code])

        current_date = fragment_end_date + timedelta(days=1)

    # Uzupełnienie brakujących danych w pozostałych kolumnach
    max_len = max(len(currency_data_dict[key]) for key in currency_data_dict.keys())
    for key in currency_data_dict.keys():
        if len(currency_data_dict[key]) < max_len:
            currency_data_dict[key].extend([np.nan] * (max_len - len(currency_data_dict[key])))

    # Tworzenie DataFrame z danymi
    df_currency = pd.DataFrame(currency_data_dict)

    return df_currency

# Funkcja do uzupełniania brakujących wartości kursów walut liniowo
def interpolate_missing_values(df, currency_columns):
    df = df.copy()

    if not pd.api.types.is_datetime64_any_dtype(df['Date']):
        df['Date'] = pd.to_datetime(df['Date'])

    for currency_column in currency_columns:
        # Uzupełniamy brakujące wartości liniowo
        df[currency_column] = df[currency_column].interpolate()

    return df
# Funkcja do obliczenia kursu złota w wybranych walutach i wygenerowania wykresu

def calculate_and_plot_gold_in_currencies(gold_df, currency_df, chosen_currencies):
    merged_df = pd.merge(gold_df, currency_df, on='Date', how='inner')
    merged_df = interpolate_missing_values(merged_df, currency_columns=chosen_currencies + ['Price'])

    result_dict = {'Date': merged_df['Date']}

    for currency in chosen_currencies:
        result_dict[currency + '_gold_price'] = merged_df['Price'] / merged_df[currency]

    result_df = pd.DataFrame(result_dict)

    plt.figure(figsize=(10, 6))

    for currency in chosen_currencies:
        plt.plot(result_df['Date'], result_df[currency + '_gold_price'], label=currency)

    plt.title('Cena złota w wybranych walutach')
    plt.xlabel('Data')
    plt.ylabel('Cena złota')
    plt.legend()
    plt.show()

    return result_df


# Ustalenie daty początkowej i końcowej
end_date =datetime.today().date()
start_date = end_date - timedelta(days=1000)

# Wywołanie funkcji dla danych o cenach złota
gold_df = process_gold_data(start_date, end_date)
if gold_df is not None:
    # print("DataFrame for gold rates (before interpolation):")
    # print(gold_df)

    # Uzupełnienie brakujących wartości kursów walut liniowo
    gold_df_interpolated = interpolate_missing_values(gold_df, currency_columns=['Price'])

 #   print("\nDataFrame for gold rates (after interpolation):")
 #   print(gold_df_interpolated)
else:
    print("No data retrieved for gold.")

# Wywołanie funkcji dla danych o kursach walut
currency_df = process_currency_data(start_date, end_date)
if currency_df is not None:
   # print("DataFrame for currency rates (before interpolation):")
    # print(currency_df)

    # Uzupełnienie brakujących wartości kursów walut liniowo
    currency_df_interpolated = interpolate_missing_values(currency_df,
                                                          currency_columns=['THB', 'USD', 'AUD', 'HKD', 'CAD', 'NZD',
                                                                            'SGD', 'EUR', 'HUF', 'CHF', 'GBP', 'UAH',
                                                                            'JPY', 'CZK', 'DKK', 'ISK', 'NOK', 'SEK',
                                                                            'RON', 'BGN', 'TRY', 'ILS', 'CLP', 'PHP',
                                                                            'MXN', 'ZAR', 'BRL', 'MYR', 'IDR', 'INR',
                                                                            'KRW', 'CNY', 'XDR'])


   # print(currency_df_interpolated)
else:
    print("No data retrieved for currency rates.")

# Wybór trzech walut
chosen_currencies = ['USD', 'EUR', 'XDR']

# Obliczenia i zwrócenie danych z cenami złota w wybranych walutach
result_df = calculate_and_plot_gold_in_currencies(gold_df_interpolated, currency_df_interpolated, chosen_currencies)

print("\nDataFrame for gold prices in chosen currencies:")
print(result_df)


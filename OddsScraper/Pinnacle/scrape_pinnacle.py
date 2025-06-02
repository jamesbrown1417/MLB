import http.client
import json
import zlib
import pandas as pd
from OddsScraper.Shared.name_utils import normalize_player_name

# API connection details
conn = http.client.HTTPSConnection("pinnacle-odds.p.rapidapi.com")

headers = {
    'x-rapidapi-key': "2cd51286d3mshb39e33e7d200279p186b64jsn2df1d1eaa2fa",  # Store this key securely
    'x-rapidapi-host': "pinnacle-odds.p.rapidapi.com",
    'Accept-Encoding': 'gzip'  # Request gzip encoding
}

# Send the GET request for MLB (sport_id=3)
conn.request("GET", "/kit/v1/special-markets?is_have_odds=true&sport_id=9", headers=headers)

# Get the response
res = conn.getresponse()
data = res.read()

# Decompress if response is gzip encoded
if res.getheader('Content-Encoding') == 'gzip':
    data = zlib.decompress(data, zlib.MAX_WBITS | 16)  # Decompress gzip

# Decode and parse JSON response
try:
    json_data = json.loads(data.decode("utf-8"))
except (json.JSONDecodeError, zlib.error) as e:
    print(f"Error processing data: {e}")
    exit()

# Function to extract player prop data
def extract_market_data(json_data, market_filter, file_name):
    market_dfs = []

    for event in json_data['specials']:
        if event['category'] == "Player Props" and market_filter in event.get('name', ''):
            name = event['name'] # Full market name like "Player Name - Hits"
            home_team = event['event']['home']
            away_team = event['event']['away']

            # Extract player name part by splitting by " - " and taking the first part
            # Default to the full name if " - " is not found, though this is less likely for player props
            player_name_extracted = name.split(' - ')[0] if ' - ' in name else name
            normalized_name = normalize_player_name(player_name_extracted)

            market_df = pd.DataFrame(event['lines']).T
            market_df['selection'] = name # Keep original full market name as 'selection'
            market_df['player_name'] = normalized_name # Add normalized player name
            market_df['home_team'] = home_team
            market_df['away_team'] = away_team
            market_dfs.append(market_df)

    if market_dfs:
        final_df = pd.concat(market_dfs, ignore_index=True)
        final_df.to_csv(f"OddsScraper/Pinnacle/{file_name}.csv", index=False)
        print(f"Saved {file_name}.csv successfully!")
    else:
        print(f"No data found for {market_filter}")

# Extract data for MLB markets

# Player Strikeouts (for pitchers)
extract_market_data(json_data, "Strikeouts", "pinnacle_strikeouts_raw")

# Player Hits
extract_market_data(json_data, "Hits", "pinnacle_hits_raw")

# Player Runs
extract_market_data(json_data, "Runs", "pinnacle_runs_raw")

# Pitching Outs
extract_market_data(json_data, "Pitching Outs", "pinnacle_pitcher_outs_raw")

# Player Home Runs
extract_market_data(json_data, "Home Run", "pinnacle_home_runs_raw")

# Player Total Bases
extract_market_data(json_data, "Total Bases", "pinnacle_total_bases_raw")

print("MLB player prop data extraction completed.")

#!/bin/bash

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/MLB || exit

# Remove .json and .txt files in specific directories
rm Data/scraped_odds/*.csv
rm OddsScraper/Bet365/Data/*.csv
rm OddsScraper/Bet365/Data/*.txt
rm OddsScraper/Neds/*.csv
rm OddsScraper/Neds/*.json

# Execute Python and R scripts for scraping
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Bet365/get_bet365_html.py
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Bet365/get_bet365_player.py
Rscript OddsScraper/Bet365/scrape_bet365.R

/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_match_json.py
Rscript OddsScraper/Neds/scrape_neds.R

/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Pinnacle/scrape_pinnacle.py
Rscript OddsScraper/Pinnacle/tidy_pinnacle.R

Rscript OddsScraper/scrape_TAB.R
Rscript OddsScraper/scrape_Sportsbet.R
Rscript OddsScraper/scrape_pointsbet.R

# Execute get arbs script
Rscript Scripts/get_arbs.R

# Publish reports using Quarto
echo "1" | quarto publish quarto-pub Reports/mlb_arbs.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main
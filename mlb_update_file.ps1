# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\R_Projects\MLB-2024"

# Remove .json and .txt files in specific directories
Remove-Item -Path "C:\Users\james\R_Projects\MLB-2024\OddsScraper\Neds\*.json"

# Execute Python and R scripts
& "C:/Python312/python.exe" "c:/Users/james/R_Projects/MLB-2024/OddsScraper/Neds/get_neds_urls.py"
& "Rscript" "OddsScraper\Neds\get_neds_match_urls.R"
& "C:/Python312/python.exe" "c:/Users/james/R_Projects/MLB-2024/OddsScraper/Neds/get_match_json.py"

# Execute R script for getting arbs
& "Rscript" "OddsScraper\master_processing_script.R"

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
$commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
git commit -m $commitMessage

# Push the commit to the 'main' branch on 'origin'
git push origin main
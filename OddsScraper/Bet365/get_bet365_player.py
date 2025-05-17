# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime
import os

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
import pandas as pd
# Read csv (no header col)
url_df = pd.read_csv('OddsScraper/Bet365/Data/urls.csv', header=None)

# Convert first column to a list
url_df = url_df[0]

# Keep only first round of URLs for now
url_df = url_df[0:15]

# Delete any existing txt files
for file in os.listdir('OddsScraper/Bet365/Data/'):
    if file.endswith('.txt') and 'h2h' not in file:
        os.remove('OddsScraper/Bet365/Data/' + file)

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")
    
    async with webdriver.Chrome(options=options) as driver:
        for index, url in enumerate(url_df, start=1): # Start counting from 1 for match_n
            try:
                await driver.get(url)
                await driver.sleep(1.1)
                
                # If there is a button that says Pitcher Strikeouts O/U, click it
                try:
                    pitcher_strikeouts_ou_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Pitcher Strikeouts O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", pitcher_strikeouts_ou_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await pitcher_strikeouts_ou_button.click()
                    await driver.sleep(1.1)
                except:
                    pass
                
                # If there is a button that says Pitcher Strikeouts, click it
                try:
                    pitcher_strikeouts_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Pitcher Strikeouts']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", pitcher_strikeouts_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await pitcher_strikeouts_button.click()
                    await driver.sleep(1.1)
                except:
                    pass
                
                # If there is a button that says Total Bases O/U, click it
                try:
                    total_bases_ou_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Total Bases O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", total_bases_ou_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await total_bases_ou_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Total Bases, click it
                try:
                    total_bases_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Total Bases']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", total_bases_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await total_bases_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Hits O/U, click it
                try:
                    hits_ou_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Hits O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", hits_ou_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await hits_ou_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Hits, click it
                try:
                    hits_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Hits']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", hits_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await hits_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Runs O/U, click it
                try:
                    runs_ou_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Runs O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", runs_ou_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await runs_ou_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Runs, click it
                try:
                    runs_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Runs']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", runs_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await runs_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Runs Batted In O/U, click it
                try:
                    rbi_ou_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Runs Batted In O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", rbi_ou_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await rbi_ou_button.click()
                    await driver.sleep(1.1)
                except:
                    pass

                # If there is a button that says Runs Batted In, click it
                try:
                    rbi_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Runs Batted In']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", rbi_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await rbi_button.click()
                    await driver.sleep(1.1)
                except:
                    pass
                
                # Get all elements with class 'msl-ShowMore_Link ' that has text 'Show more'
                button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]")
                
                print(len(button_elements))
                    
                # Scroll into view of each button, click it and wait 1 second
                for button_element in button_elements:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await button_element.click()
                    await driver.sleep(1.1)
                    
                # Write out html to file------------------------------------------------
                # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players = await elem.get_attribute('outerHTML')
                with open(f"OddsScraper/Bet365/Data/body_html_players_match_{index}.txt", 'w') as f:
                    f.write(body_html_players)
                        
            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())                    
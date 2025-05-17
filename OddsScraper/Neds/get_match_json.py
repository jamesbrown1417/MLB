from playwright.async_api import async_playwright
import asyncio
import pandas as pd
import json
import os

async def main():
    # Load URLs from the CSV file
    try:
        match_urls_df = pd.read_csv("OddsScraper/Neds/neds_mlb_match_urls.csv")
        urls = match_urls_df["url"].tolist()
        if not urls:
            print("No URLs found in the CSV file. Exiting.")
            return
    except FileNotFoundError:
        print("Error: neds_mlb_match_urls.csv not found in OddsScraper/Neds/. Exiting.")
        return
    except Exception as e:
        print(f"Error reading or parsing CSV file: {e}. Exiting.")
        return

    # Ensure the output directory exists
    output_dir = "OddsScraper/Neds/"
    os.makedirs(output_dir, exist_ok=True)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True) # You have headless=False, good for observation
        page = await browser.new_page()

        # Counter to generate unique file names
        file_counter = 1

        # Listen to responses instead of requests
        async def handle_response(response):
            nonlocal file_counter # Ensure we're using the file_counter from the main scope
            request = response.request # Get the request object from the response
            if 'card' in request.url: # Check if 'card' is in the request URL associated with the response
                try:
                    print(f"Found 'card' in response URL: {request.url}")
                    json_body = await response.json()
                    file_path = os.path.join(output_dir, f"data_{file_counter}.json")
                    with open(file_path, 'w', encoding='utf-8') as f:
                        json.dump(json_body, f, ensure_ascii=False, indent=4)
                    print(f"Saved JSON to {file_path}")
                    file_counter += 1
                except json.JSONDecodeError:
                    # If the response is not valid JSON, read as text
                    try:
                        text_body = await response.text()
                        print(f"Warning: Could not parse JSON from {request.url}. Content was: {text_body[:200]}...") # Log first 200 chars
                    except Exception as e_text:
                        print(f"Error reading non-JSON response body from {request.url}: {e_text}")
                except Exception as e:
                    print(f"Error processing response from {request.url}: {e}")

        # Register the response handler
        page.on('response', handle_response)

        # Process each URL
        for i, url in enumerate(urls):
            print(f"Processing URL {i+1}/{len(urls)}: {url}")
            try:
                await page.goto(url, wait_until="domcontentloaded", timeout=60000) # wait_until can be 'load' or 'domcontentloaded'
                print(f"Page {url} loaded. Waiting for element with class 'market-two-col__entrant-name'...")

                # Wait for the specific class to be visible on the page
                # Playwright's default timeout for wait_for_selector is 30 seconds.
                # You can adjust it with the timeout parameter (e.g., timeout=60000 for 60s)
                await page.wait_for_selector(".market-two-col__entrant-name", timeout=30000)
                print(f"Element '.market-two-col__entrant-name' found on {url}.")

                # If you need to do other things after the element is found, before going to the next URL,
                # you can add them here. For example, extract some data or take a screenshot.

            except Exception as e: # Catching Playwright's TimeoutError or other navigation/wait errors
                print(f"Error processing URL {url}: {e}")
                print(f"Skipping to next URL if any.")
                continue # Move to the next URL if an error occurs

        print("Finished processing all URLs.")
        print("Closing browser...")
        await browser.close()
        print("Browser closed.")

if __name__ == "__main__":
    asyncio.run(main())
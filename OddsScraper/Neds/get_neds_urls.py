import asyncio
from playwright.async_api import async_playwright
import os

# The URL pattern we are interested in
url_pattern = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%2202721435-4671-4cd0-98f7-15d41ee4103e%22%5D"

# Define output path robustly
output_dir = os.path.join("OddsScraper", "Neds")
output_file = os.path.join(output_dir, "neds_response.json")

async def main():
    response_captured_event = asyncio.Event()
    handler_context = {"page_instance": None, "listener_removed": False}

    async def handle_response(response):
        if response_captured_event.is_set() or handler_context["listener_removed"]:
            return

        if response.url == url_pattern:
            page_instance = handler_context.get("page_instance")
            print(f"First matching response found: {response.url}")
            try:
                body = await response.body()
                os.makedirs(output_dir, exist_ok=True)
                with open(output_file, "w", encoding="utf-8") as f:
                    f.write(body.decode("utf-8"))
                print(f"Response captured and written to {output_file}!")
                
                response_captured_event.set() # Signal that we've got what we need

                if page_instance and not page_instance.is_closed():
                    try:
                        page_instance.remove_listener('response', handle_response)
                        handler_context["listener_removed"] = True
                        print("Response listener removed.")
                    except Exception as e_rem:
                        print(f"Note: Could not remove listener from within handler: {e_rem}")
            except Exception as e:
                print(f"Error processing response body or writing file: {e}")
                if not response_captured_event.is_set():
                    response_captured_event.set() # Ensure main flow can unblock

    async with async_playwright() as p:
        browser = None
        page = None # Assign page here for access in finally
        navigation_task = None
        event_wait_task = None
        
        try:
            browser = await p.chromium.launch()
            context = await browser.new_context()
            page = await context.new_page()
            handler_context["page_instance"] = page

            page.on('response', handle_response)

            print(f"Starting navigation to https://www.neds.com.au/sports/baseball/usa/mlb and listening for XHR...")
            
            # Create a task for page navigation
            # You can adjust wait_until and timeout. Using "load" or "domcontentloaded"
            # might be faster if networkidle isn't strictly needed for the XHR to appear.
            navigation_timeout = 90000 # 90 seconds for goto, but we hope to cancel it earlier
            navigation_task = asyncio.create_task(
                page.goto(
                    "https://www.neds.com.au/sports/baseball/usa/mlb",
                    wait_until="networkidle", # Consider "load" for faster script completion if XHR appears early
                    timeout=navigation_timeout
                ),
                name="NavigationTask"
            )
            
            # Create a task for waiting for your specific response event
            event_wait_task = asyncio.create_task(
                response_captured_event.wait(),
                name="EventWaitTask"
            )

            # Wait for EITHER the response to be captured OR navigation to complete/timeout
            done, pending = await asyncio.wait(
                {navigation_task, event_wait_task},
                return_when=asyncio.FIRST_COMPLETED
            )

            if event_wait_task in done:
                print("Specific response was captured!")
                # If navigation is still pending, cancel it as we have what we need.
                if not navigation_task.done():
                    print("Cancelling in-progress navigation task.")
                    navigation_task.cancel()
            elif navigation_task in done:
                print("Navigation task finished (or timed out) before specific response was captured.")
                try:
                    await navigation_task # To raise and catch any navigation errors
                    print("Navigation completed successfully (but desired XHR not found first).")
                except asyncio.CancelledError:
                    # This case should not happen if navigation_task is in 'done' unless externally cancelled
                    print("Navigation task was unexpectedly cancelled.")
                except Exception as nav_exc:
                    print(f"Navigation failed or timed out: {nav_exc.__class__.__name__} - {nav_exc}")
            
            # Ensure all tasks are properly "awaited" or cancelled to prevent warnings
            for task_in_pending in pending: # Should be at most one task here
                if not task_in_pending.done():
                    task_in_pending.cancel()
            
            # Await all tasks to allow cancellations to process and gather any exceptions
            all_tasks = [navigation_task, event_wait_task]
            results = await asyncio.gather(*all_tasks, return_exceptions=True)
            for i, result_item in enumerate(results):
                task_name = all_tasks[i].get_name() if hasattr(all_tasks[i], 'get_name') else f"Task-{i}"
                if isinstance(result_item, asyncio.CancelledError):
                    print(f"{task_name} was successfully cancelled.")
                elif isinstance(result_item, Exception):
                    print(f"{task_name} resulted in an error: {result_item}")

        except Exception as e_main:
            print(f"An error occurred in the main Playwright block: {e_main}")
        finally:
            print("Proceeding to cleanup...")
            # Clean up the listener if it wasn't already removed by the handler
            if page and not page.is_closed() and not handler_context["listener_removed"]:
                try:
                    page.remove_listener('response', handle_response)
                    print("Response listener cleaned up in finally block.")
                except Exception:
                    pass # Ignore errors during this final cleanup attempt

            if page and not page.is_closed():
                print("Closing page...")
                try:
                    await page.close()
                    print("Page closed.")
                except Exception as e_page_close:
                    print(f"Note: Error closing page (may be ok if browser is already closing): {e_page_close}")
            
            if browser and browser.is_connected():
                print("Closing browser...")
                await browser.close()
                print("Browser closed.")
            print("Playwright operations finished.")

if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("\nScript interrupted by user.")
    finally:
        print("Script execution ended.")
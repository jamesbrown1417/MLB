import re

# Attempt to import unidecode, otherwise use a manual mapping
try:
    from unidecode import unidecode
    UNIDECODE_AVAILABLE = True
except ImportError:
    UNIDECODE_AVAILABLE = False
    ACCENT_MAP = {
        'á': 'a', 'é': 'e', 'í': 'i', 'ó': 'o', 'ú': 'u', 'ü': 'u', 'ñ': 'n',
        'Á': 'A', 'É': 'E', 'Í': 'I', 'Ó': 'O', 'Ú': 'U', 'Ü': 'U', 'Ñ': 'N',
        'à': 'a', 'è': 'e', 'ì': 'i', 'ò': 'o', 'ù': 'u',
        'À': 'A', 'È': 'E', 'Ì': 'I', 'Ò': 'O', 'Ù': 'U',
        'â': 'a', 'ê': 'e', 'î': 'i', 'ô': 'o', 'û': 'u',
        'Â': 'A', 'Ê': 'E', 'Î': 'I', 'Ô': 'O', 'Û': 'U',
        'ä': 'a', 'ë': 'e', 'ï': 'i', 'ö': 'o',
        'Ä': 'A', 'Ë': 'E', 'Ï': 'I', 'Ö': 'O', # Ü already covered
        'ã': 'a', 'õ': 'o',
        'Ã': 'A', 'Õ': 'O',
        'ç': 'c', 'Ç': 'C',
        # Add more mappings if necessary
    }

def normalize_player_name(name_str: str) -> str:
    """
    Normalizes a player's name by lowercasing, removing accents,
    trimming whitespace, reducing multiple spaces, and converting to title case.

    Args:
        name_str: The input player name string.

    Returns:
        A normalized player name string.
    """
    if name_str is None or not isinstance(name_str, str) or not name_str.strip():
        return ""

    # 1. Convert to lowercase
    processed_name = name_str.lower()

    # 2. Replace accented characters
    if UNIDECODE_AVAILABLE:
        processed_name = unidecode(processed_name)
    else:
        for accented_char, replacement_char in ACCENT_MAP.items():
            # Ensure we are replacing lowercase accented chars correctly after initial tolower()
            if accented_char.lower() in processed_name:
                 processed_name = processed_name.replace(accented_char.lower(), replacement_char.lower())

    # 3. Remove any leading or trailing whitespace
    processed_name = processed_name.strip()

    # 4. Reduce multiple spaces between name parts to a single space
    processed_name = re.sub(r'\s+', ' ', processed_name)

    # 5. Convert the name to title case
    # A simple title() might not work for names like O'Malley or McDonald.
    # However, for "firstname lastname" -> "Firstname Lastname" it's usually fine.
    # A more robust solution might involve splitting by space and capitalizing each part,
    # but that can also have issues with particles like "de la".
    # For this implementation, we'll use a common approach that handles simple cases.

    # Python's str.title() method correctly handles apostrophes (e.g. O'Hearn)
    # and capitalizes the first letter of each word.
    processed_name = processed_name.title()

    return processed_name

if __name__ == '__main__':
    # Test cases
    names_to_test = [
        ("  José  Altuve  ", "Jose Altuve"),
        ("Adrián Beltré", "Adrian Beltre"),
        ("Yuli Gurriel", "Yuli Gurriel"),
        ("GIANCARLO STANTON", "Giancarlo Stanton"),
        ("Jean-Luc Picard", "Jean-Luc Picard"), # Test hyphen
        ("Víctor Martínez", "Victor Martinez"),
        ("Néstor Cortés Jr.", "Nestor Cortes Jr."), # Test period and suffix
        ("  leading space", "Leading Space"),
        ("trailing space  ", "Trailing Space"),
        ("multiple   spaces", "Multiple Spaces"),
        (None, ""),
        ("", ""),
        (" ", ""),
        ("áéíóúüñÁÉÍÓÚÜÑçÇ", "AeiouunAeiouuncc"), # Test accent map if unidecode is not available
        ("Javier Báez", "Javier Baez"),
        ("Ryan O'Hearn", "Ryan O'Hearn")
    ]

    for original, expected in names_to_test:
        normalized = normalize_player_name(original)
        print(f"Original: '{original}' -> Normalized: '{normalized}' (Expected: '{expected}') -> Correct: {normalized == expected}")

    if UNIDECODE_AVAILABLE:
        print("\nUsing unidecode library.")
    else:
        print("\nUsing manual accent map (unidecode not available).")

    # Example of a name that might be tricky for simple title() if not for unidecode's prior work
    # or if it had lowercase particles like "de la"
    print(normalize_player_name("pedro de la rosa")) # Expected: Pedro De La Rosa (standard title())
                                                    # or Pedro de la Rosa (if more advanced title casing was used)
                                                    # Current function will produce "Pedro De La Rosa"
    print(normalize_player_name("DAVID MCGOLD")) # Expected: David Mcgold (standard title())
                                                 # Current function will produce "David Mcgold" - this is a known limitation of .title() for "Mc" names
                                                 # A more specific title casing logic would be needed for "Mc"
                                                 # For now, the requirement is "Firstname Lastname" capitalization which .title() handles.
    print(normalize_player_name("DAVID MCDONALD")) # Expected: David Mcdonald (standard title())
                                                   # Current function: David Mcdonald
    print(normalize_player_name("o'neill")) # Expected: O'Neill
                                            # Current function: O'Neill (str.title() handles this)

    # Test the manual map directly if unidecode is not available
    if not UNIDECODE_AVAILABLE:
        manual_test_name = "Jörg Müller"
        expected_manual = "Jorg Muller"
        # Simulate the steps
        p = manual_test_name.lower()
        for accented_char, replacement_char in ACCENT_MAP.items():
            if accented_char.lower() in p:
                 p = p.replace(accented_char.lower(), replacement_char.lower())
        p = p.strip()
        p = re.sub(r'\s+', ' ', p)
        p = p.title()
        print(f"Manual test: '{manual_test_name}' -> Normalized: '{p}' (Expected: '{expected_manual}') -> Correct: {p == expected_manual}")

        manual_test_name_2 = "françois truffaut"
        expected_manual_2 = "Francois Truffaut"
        p2 = manual_test_name_2.lower()
        for accented_char, replacement_char in ACCENT_MAP.items():
            if accented_char.lower() in p2:
                 p2 = p2.replace(accented_char.lower(), replacement_char.lower())
        p2 = p2.strip()
        p2 = re.sub(r'\s+', ' ', p2)
        p2 = p2.title()
        print(f"Manual test 2: '{manual_test_name_2}' -> Normalized: '{p2}' (Expected: '{expected_manual_2}') -> Correct: {p2 == expected_manual_2}")

        # Test if accents in ACCENT_MAP are correctly lowercased during replacement
        # when the input name_str is initially all uppercase.
        # e.g. "ÁÉÍÓÚ" -> "aeiou" (lowercase unaccented) -> "Aeiou" (title case)
        uppercase_accented_name = "JOSÉ ÁLVAREZ"
        expected_uppercase_accented = "Jose Alvarez"
        p3 = uppercase_accented_name.lower() # "josé álvarez"
        for accented_char, replacement_char in ACCENT_MAP.items():
            # accented_char.lower() ensures we match against the lowercased processed_name
            # replacement_char.lower() ensures we replace with a lowercase unaccented char
            if accented_char.lower() in p3:
                 p3 = p3.replace(accented_char.lower(), replacement_char.lower())
        p3 = p3.strip()
        p3 = re.sub(r'\s+', ' ', p3)
        p3 = p3.title()
        print(f"Uppercase accented test: '{uppercase_accented_name}' -> Normalized: '{p3}' (Expected: '{expected_uppercase_accented}') -> Correct: {p3 == expected_uppercase_accented}")

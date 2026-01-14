import json
try:
    with open('/home/heefoo/.gemini/settings.json', 'r') as f:
        json.load(f)
    print("JSON is valid")
except json.JSONDecodeError as e:
    print(f"JSON error: {e}")

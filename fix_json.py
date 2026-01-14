import sys

path = '/home/heefoo/.gemini/settings.json'
with open(path, 'r') as f:
    content = f.read()

# The problematic sequence: \", \"));
# We want: \", \"\"));

old_str = r'get(ENV, \"JULIA_DEPOT_PATH\", \""));'
new_str = r'get(ENV, \"JULIA_DEPOT_PATH\", \"\"));'

if old_str in content:
    new_content = content.replace(old_str, new_str)
    with open(path, 'w') as f:
        f.write(new_content)
    print("Fixed JSON file.")
else:
    print("Could not find the target string to fix.")

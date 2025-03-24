#!/usr/bin/env python3
import re

##############################################################################
# 1. Global Parameters (like defglobal)
##############################################################################
MIN_PREMISE_AREA       = 65.0
MIN_FLOOR_AREA         = 150.0
MAX_PREMISES_PER_FLOOR = 5

##############################################################################
# 2. In-Memory “Facts” Storage
##############################################################################
# Each fact is stored as a dict:
#   {
#     "fact_id": int,           # e.g. f-1
#     "fact_type": "building" or "premise",
#     "slots": { slotName: value, ... },
#     "active": bool
#   }
FACTS = []
NEXT_FACT_ID = 1

##############################################################################
# 3. Helper Functions (like deffunction)
##############################################################################

def find_premises():
    return [f for f in FACTS if f["active"] and f["fact_type"] == "premise"]

def find_buildings():
    return [f for f in FACTS if f["active"] and f["fact_type"] == "building"]

def find_premise_by_id(pid):
    for f in FACTS:
        if f["active"] and f["fact_type"] == "premise":
            if f["slots"].get("id") == pid:
                return f
    return None

def total_premises_area(premise_ids):
    total = 0.0
    for pid in premise_ids:
        p = find_premise_by_id(pid)
        if p is not None:
            total += p["slots"].get("area", 0.0)
    return total

def count_premises_on_floor(floor_number):
    return sum(1 for p in find_premises() if p["slots"].get("floor") == floor_number)

##############################################################################
# 4. The “Rules”
##############################################################################

def synchronize_building_premises():
    """
    Automatically update each building’s (premises-list) based on active premises
    whose (building-id) matches the building’s name.
    """
    for b in find_buildings():
        bname = b["slots"].get("building-name", "")
        matched = []
        for p in find_premises():
            if p["slots"].get("building-id") == bname:
                pid = p["slots"].get("id")
                matched.append(pid)
        matched.sort()
        old_list = b["slots"].get("premises-list", [])
        if sorted(old_list) != matched:
            b["slots"]["premises-list"] = matched

def update_building_total_area():
    for b in find_buildings():
        old_area = b["slots"].get("total-area", 0.0)
        plist = b["slots"].get("premises-list", [])
        new_area = total_premises_area(plist)
        if abs(new_area - old_area) > 1e-9:
            b["slots"]["total-area"] = new_area
            bname = b["slots"].get("building-name", "?")
            print(f"Будівля {bname} оновлена, нова загальна площа: {new_area}")
        else:
            bname = b["slots"].get("building-name", "?")
            print(f"Будівля {bname} має незмінну загальну площу: {old_area}")

def check_premise_area():
    for p in find_premises():
        area = p["slots"].get("area", 0.0)
        if area < MIN_PREMISE_AREA:
            pid = p["slots"].get("id", "?")
            ut  = p["slots"].get("usage-type", "?")
            print(f"Приміщення {pid} ({ut}) має недостатню площу: {area} кв.м.")

def check_building_floor_area():
    for b in find_buildings():
        bname  = b["slots"].get("building-name", "?")
        floors = b["slots"].get("floors", 1)
        ta     = b["slots"].get("total-area", 0.0)
        if floors == 0:
            print(f"Будівля {bname} має 0 поверхів - некоректні дані.")
            continue
        avg = ta / floors
        if avg < MIN_FLOOR_AREA:
            print(f"Будівля {bname} має недостатню середню площу на поверх: {avg} кв.м.")
        else:
            print(f"Будівля {bname} відповідає нормам, середня площа на поверх: {avg} кв.м.")

def check_premises_per_floor():
    floor_counts = {}
    for p in find_premises():
        fl = p["slots"].get("floor", 0)
        floor_counts[fl] = floor_counts.get(fl, 0) + 1
    for fl, cnt in floor_counts.items():
        if cnt > MAX_PREMISES_PER_FLOOR:
            print(f"На поверсі {fl} перевищено допустиму кількість приміщень: {cnt}")

def check_fire_safety():
    """
    Additional rule: For buildings with 4 or more floors, ensure there are at least 2 emergency exits.
    The slot 'emergency-exits' is checked on building facts.
    """
    for b in find_buildings():
        floors = b["slots"].get("floors", 0)
        if floors >= 4:
            exits = b["slots"].get("emergency-exits", 0)
            if exits < 2:
                bname = b["slots"].get("building-name", "?")
                print(f"Будівля {bname} може не відповідати вимогам пожежної безпеки: emergency-exits = {exits}")

def run_rules():
    synchronize_building_premises()
    update_building_total_area()
    check_premise_area()
    check_building_floor_area()
    check_premises_per_floor()
    check_fire_safety()

# For extended functionality, we maintain a RULES dictionary.
RULES = {
    "update-building-total-area": "Оновлює загальну площу будівлі на основі площ приміщень.",
    "check-premise-area": "Перевіряє, чи має кожне приміщення мінімальну площу.",
    "check-building-floor-area": "Перевіряє, чи середня площа на поверх відповідає нормам.",
    "check-premises-per-floor": "Перевіряє, чи не перевищено максимальну кількість приміщень на одному поверсі.",
    "check-fire-safety": "Перевіряє вимоги пожежної безпеки (emergency-exits) для будівель з ≥4 поверхами."
}

def handle_rules():
    print("Loaded Rules:")
    for name, desc in RULES.items():
        print(f"  {name}: {desc}")

def handle_agenda():
    print("Agenda is empty. All rules have been fired.")

##############################################################################
# 5. Fact Management: (assert), (retract), (facts)
##############################################################################

def add_fact(fact_type, slots):
    global NEXT_FACT_ID
    fact_id = NEXT_FACT_ID
    NEXT_FACT_ID += 1
    new_fact = {
        "fact_id": fact_id,
        "fact_type": fact_type,
        "slots": slots,
        "active": True
    }
    FACTS.append(new_fact)
    return fact_id

def remove_fact_by_id(fact_id):
    for f in FACTS:
        if f["active"] and f["fact_id"] == fact_id:
            f["active"] = False
            return True
    return False

def print_facts():
    actives = [f for f in FACTS if f["active"]]
    print(f"For a total of {len(actives)} facts.")
    for f in actives:
        fid = f"f-{f['fact_id']}"
        if f["fact_type"] == "building":
            slot_strs = []
            for k, v in f["slots"].items():
                slot_strs.append(f"({k} {clips_repr(k, v)})")
            print(f"{fid}  (building {' '.join(slot_strs)})")
        elif f["fact_type"] == "premise":
            slot_strs = []
            for k, v in f["slots"].items():
                slot_strs.append(f"({k} {clips_repr(k, v)})")
            print(f"{fid}  (premise {' '.join(slot_strs)})")

def clips_repr(key, value):
    if isinstance(value, (int, float)):
        return str(value)
    elif isinstance(value, list):
        return " ".join(str(x) for x in value)
    else:
        s = str(value)
        if key == "usage-type":
            return f"\"{s}\""
        if re.match(r"^[A-Za-z0-9_-]+$", s):
            return s
        return f"\"{s}\""

##############################################################################
# 6. S-expression Parser & REPL with CLIPS-like Error Messages
##############################################################################

def tokenize(line):
    line = line.replace("(", " ( ").replace(")", " ) ")
    return re.findall(r'"[^"]*"|[^\s]+', line)

def parse_tokens(tokens, i=0):
    expr = []
    while i < len(tokens):
        tok = tokens[i]
        if tok == "(":
            sub, new_i = parse_tokens(tokens, i+1)
            expr.append(sub)
            i = new_i
        elif tok == ")":
            return expr, i+1
        else:
            if tok[0] == '"' and tok[-1] == '"':
                expr.append(tok[1:-1])
            else:
                expr.append(tok)
            i += 1
    return expr, i

def parse_sexp(text):
    tokens = tokenize(text)
    exprs = []
    i = 0
    while i < len(tokens):
        if tokens[i] == "(":
            sx, j = parse_tokens(tokens, i+1)
            exprs.append(sx)
            i = j
        else:
            i += 1
    return exprs

def maybe_numeric(val):
    try:
        return int(val)
    except:
        pass
    try:
        return float(val)
    except:
        pass
    return val

def handle_assert(expr):
    if len(expr) == 1 and isinstance(expr[0], list):
        expr = expr[0]
    if not expr:
        print("[ARGACCES1] Function 'assert' expected exactly 1 argument.")
        return
    fact_type = expr[0]
    slots = {}
    for slot_expr in expr[1:]:
        if not isinstance(slot_expr, list) or len(slot_expr) == 0:
            continue
        slot_name = slot_expr[0]
        if slot_name == "premises-list":
            if len(slot_expr) > 1:
                vals = [maybe_numeric(x) for x in slot_expr[1:]]
                slots[slot_name] = vals
            else:
                slots[slot_name] = []
        else:
            if len(slot_expr) >= 2:
                raw_val = slot_expr[1]
                conv_val = maybe_numeric(raw_val)
                slots[slot_name] = conv_val
            else:
                slots[slot_name] = None
    fid = add_fact(fact_type, slots)
    print(f"<Fact-{fid}>")

def handle_retract(expr):
    if len(expr) != 1:
        print("[ARGACCES1] Function 'retract' expected exactly 1 argument.")
        return
    try:
        fid = int(expr[0])
        ok = remove_fact_by_id(fid)
        if not ok:
            print(f"[FACTERROR] No active fact with ID {fid}.")
    except ValueError:
        print("[ARGACCES1] Function 'retract' expected exactly 1 argument.")

def handle_reset():
    global FACTS, NEXT_FACT_ID
    FACTS = []
    NEXT_FACT_ID = 1
    load_initial_facts()
    print("Environment reset. Initial facts loaded.")

def read_multiline():
    lines = []
    # Print the prompt only for the first line.
    first_line = input("CLIPS> ")
    lines.append(first_line)
    while True:
        text = "\n".join(lines)
        # When the number of "(" is less than or equal to ")", input is complete.
        if text.count("(") <= text.count(")"):
            break
        # For subsequent lines, use an empty prompt.
        next_line = input("")
        lines.append(next_line)
    return "\n".join(lines)


def repl():
    # Print the header exactly as in a real CLIPS session.
    print("       CLIPS (6.4.2 1/14/25)")
    while True:
        try:
            text = read_multiline().strip()
        except EOFError:
            break
        if not text:
            continue
        if text.lower() in ("quit", "exit"):
            break
        exprs = parse_sexp(text)
        if not exprs:
            continue
        for sx in exprs:
            if not sx:
                continue
            cmd = sx[0]
            args = sx[1:]
            if cmd == "facts":
                if args:
                    print("[ARGACCES1] Function 'facts' expected exactly 0 arguments.")
                else:
                    print_facts()
            elif cmd == "run":
                if args:
                    print("[ARGACCES1] Function 'run' expected exactly 0 arguments.")
                else:
                    run_rules()
            elif cmd == "assert":
                if not args:
                    print("[ARGACCES1] Function 'assert' expected exactly 1 argument.")
                else:
                    handle_assert(args[0])
            elif cmd == "retract":
                handle_retract(args)
            elif cmd == "reset":
                if args:
                    print("[ARGACCES1] Function 'reset' expected exactly 0 arguments.")
                else:
                    handle_reset()
            elif cmd == "rules":
                if args:
                    print("[ARGACCES1] Function 'rules' expected exactly 0 arguments.")
                else:
                    handle_rules()
            elif cmd == "agenda":
                if args:
                    print("[ARGACCES1] Function 'agenda' expected exactly 0 arguments.")
                else:
                    handle_agenda()
            elif cmd == "help":
                if args:
                    print("[ARGACCES1] Function 'help' expected exactly 0 arguments.")
                else:
                    print("Available commands:")
                    print("  (facts)       - Show all active facts.")
                    print("  (run)         - Fire all rules.")
                    print("  (assert ...)  - Add a fact (e.g., (assert (premise (id P103) ...))).")
                    print("  (retract N)   - Remove fact with ID N.")
                    print("  (reset)       - Reset environment and load initial facts.")
                    print("  (rules)       - List all loaded rules.")
                    print("  (agenda)      - Show pending rules (agenda).")
                    print("  (help)        - Display this help message.")
                    print("  (quit)        - Exit the system.")
            else:
                print(f"[ARGACCES1] Function '{cmd}' expected exactly 1 argument.")

##############################################################################
# 7. Initial Facts Setup (like deffacts)
##############################################################################

def load_initial_facts():
    # building B1 with emergency exits defined for fire safety
    add_fact("building", {
        "building-name": "B1",
        "year-built": 2020,
        "floors": 5,
        "total-area": 175.0,
        "premises-list": ["P101", "P102"],
        "emergency-exits": 2
    })
    add_fact("premise", {
        "id": "P101",
        "building-id": "B1",
        "floor": 1,
        "area": 75.0,
        "usage-type": "ОФІС"
    })
    add_fact("premise", {
        "id": "P102",
        "building-id": "B1",
        "floor": 1,
        "area": 100.0,
        "usage-type": "МАГАЗИН"
    })
    # building B2 without sufficient emergency exits to trigger fire safety warning
    add_fact("building", {
        "building-name": "B2",
        "year-built": 2015,
        "floors": 3,
        "total-area": 0.0,
        "premises-list": [],
        "emergency-exits": 0
    })
    add_fact("premise", {
        "id": "P201",
        "building-id": "B2",
        "floor": 1,
        "area": 60.0,
        "usage-type": "ОФІС"
    })
    add_fact("premise", {
        "id": "P202",
        "building-id": "B2",
        "floor": 1,
        "area": 120.0,
        "usage-type": "МАГАЗИН"
    })

##############################################################################
# 8. Main
##############################################################################

def main():
    handle_reset()  # Load initial facts automatically.
    repl()          # Enter the REPL.

if __name__ == "__main__":
    main()

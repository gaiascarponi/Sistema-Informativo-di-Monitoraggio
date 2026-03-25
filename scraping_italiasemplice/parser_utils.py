import re
from selenium.webdriver.common.by import By

def extract_label(body_text, label):
    m = re.search(rf"{label}:\s*(.*)", body_text, re.IGNORECASE)
    if m:
        return m.group(1).split('\n')[0].strip()
    return ""

def extract_procedure_name(driver):
    nome_proc = "N/D"
    try:
        el = driver.find_element(By.XPATH, "//div[contains(@class, 'card-body')]//h3 | //div[contains(@class, 'card-body')]//h5 | //div[contains(@class, 'card-header')]//h5")
        nome_proc = el.text.strip()
    except:
        try:
            full_text = driver.find_element(By.TAG_NAME, "body").text
            parts = full_text.split("Dettaglio Procedura")[1].split("Settore:")[0].split('\n')
            valid_lines = [p.strip() for p in parts if p.strip() and "Scarica" not in p]
            if valid_lines: nome_proc = valid_lines[0]
        except: pass
    return nome_proc.replace("Scarica XLSX/CSV", "").strip()

def parse_interventions(driver):
    interventions_list = []
    cards = driver.find_elements(By.XPATH, "//div[contains(@class, 'card') and .//*[contains(text(), 'Tipologia di intervento')]]")
    
    if not cards:
        return [{"Intervento": "", "Anno": "", "Descrizione Intervento": "", "Tipo Intervento": "", "Natura Intervento": "", "Riferimenti": ""}]

    for card in cards:
        tipo = natura = riferimenti = anno = title_clean = ""
        desc = []
        
        try:
            card_text = card.text
            lines = [l.strip() for l in card_text.split('\n') if l.strip()]
            def get_val(keyword):
                for i, line in enumerate(lines):
                    if keyword.lower() in line.lower() and i + 1 < len(lines):
                        res = lines[i+1]
                        if res.lower() == keyword.lower() and i + 2 < len(lines): #to avoid cases where value is missing and keyword is repeated
                            return lines[i+2]
                        return res
                return ""
            tipo = get_val("Tipologia di intervento")
            natura = get_val("Natura intervento")
            riferimenti = get_val("Riferimenti")

            try:
                title_el = card.find_element(By.TAG_NAME, "h5")
                title_full = title_el.text.strip()
            except:
                title_full = lines[0] if lines else ""
            anno_m = re.search(r'\((\d{4})\)', title_full)
            if anno_m:
                anno = anno_m.group(1)
                title_clean = title_full.replace(f"({anno})", "").strip()
            else:
                title_clean = title_full
            start_capture = False
            for line in lines:
                if line == title_full:
                    start_capture = True; continue
                if any(k in line for k in ["Tipologia di intervento", "Natura intervento", "Riferimenti"]):
                    break
                if start_capture: desc.append(line)

            interventions_list.append({
                "Intervento": title_clean, "Anno": anno,
                "Descrizione Intervento": " ".join(desc).strip(),
                "Tipo Intervento": tipo, "Natura Intervento": natura, "Riferimenti": riferimenti
            })
        except:
            interventions_list.append({"Intervento": "Errore parsing"})
            
    return interventions_list

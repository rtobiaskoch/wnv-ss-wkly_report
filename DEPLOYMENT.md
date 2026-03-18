# Deployment & Sharing Guide

This document describes options for running the pipeline without a local R install
and for giving collaborators access to the Google Sheets databases.

---

## 1  Giving Others Access to Google Sheets

There are two approaches depending on what access level collaborators need.

### A  Manual share (easiest — no code changes)

1. Open each Google Sheet listed in `config/gsheet_keys.R`.
2. Click **Share** → add the collaborator's Google email → choose **Viewer** or **Editor**.
3. Repeat for each sheet they need.

That's it. They can now open the sheets in their browser and read/edit directly.
The pipeline does not need to be involved for manual edits.

### B  Service account (for automated or headless use)

Use this when running the pipeline in CI/CD, on a server, or via shinyapps.io
— any context where nobody can click an OAuth pop-up.

1. Follow the steps in `config/gauth_service_account_TEMPLATE.R` to create a
   GCP service account and download its JSON key.
2. Share each Google Sheet with the **service account email**
   (e.g. `wnv-pipeline-bot@your-project.iam.gserviceaccount.com`).
3. Store the JSON key path in the env var `GOOGLE_APPLICATION_CREDENTIALS`.
4. Add this block near the top of the pipeline `.qmd` (or `load_packages.R`):

```r
sa_key <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = "")
if (nchar(sa_key) > 0 && file.exists(sa_key)) {
  googledrive::drive_auth(path = sa_key)
  googlesheets4::gs4_auth(path = sa_key)
} else {
  googledrive::drive_auth()
  googlesheets4::gs4_auth(token = googledrive::drive_token())
}
```

---

## 2  Web Hosting Options

### Option A — shinyapps.io (historical visualizer — easiest, free tier)

The existing `shiny_hx_stats/app.R` lets users upload a CSV and explore
historical abundance / PIR / VI data interactively.

**Deploy in 3 steps:**

```r
install.packages("rsconnect")
# Set credentials once (get token from shinyapps.io → Account → Tokens):
rsconnect::setAccountInfo(name="YOUR_NAME", token="TOKEN", secret="SECRET")
# Deploy:
source("shiny_hx_stats/deploy_shinyapps.R")
```

- Free tier: 25 active hours/month (plenty for a small team)
- Paid tier: unlimited hours, custom domain
- URL will be: `https://YOUR_NAME.shinyapps.io/wnv-mosquito-surveillance`

No R install needed for end users — they just visit the URL.

---

### Option B — GitHub Actions automated pipeline (recommended for weekly runs)

The workflow at `.github/workflows/weekly_report.yml` runs the full pipeline
every Monday automatically and saves the Excel report + HTML as GitHub
**Artifacts** (downloadable from the Actions tab).

**Setup:**

1. Push this repo to GitHub if not already there.
2. Go to **Settings → Secrets → Actions** and add:
   - `GOOGLE_SERVICE_ACCOUNT_JSON` — paste the entire contents of your service account JSON key
   - Optional: override any sheet ID (see the workflow file for names)
3. Go to **Actions → Weekly WNV/SLEV Report → Run workflow** to trigger manually,
   or just wait for Monday.

Artifacts are retained for 90 days. Anyone with repo access can download them.

---

### Option C — Quarto Pub (static HTML report — read-only, public)

Publish the rendered HTML report as a public webpage:

```bash
# After rendering the pipeline:
quarto publish quarto-pub wnv-s_weekly_report_pipeline_v2.qmd
```

- Free at https://quartopub.com
- Updates each time you republish
- No auth needed — anyone with the URL can view it
- Good for sharing the weekly report PDF/HTML with stakeholders

---

### Option D — Docker (full reproducibility for power users)

A Docker image pins the exact R version and all packages so anyone can
reproduce results identically on any machine.

**Quick start (once a Dockerfile is added):**

```bash
docker build -t wnv-pipeline .
docker run --rm \
  -v $(pwd)/1_input:/app/1_input \
  -v $(pwd)/3_output:/app/3_output \
  -e GOOGLE_APPLICATION_CREDENTIALS=/secrets/sa_key.json \
  -v ~/.config/wnv_sa_key.json:/secrets/sa_key.json \
  wnv-pipeline \
  Rscript config/config_weekly.R --week 28 --year 2025 --download T
```

See `renv` below for capturing package versions to include in the image.

---

## 3  Reproducibility: Locking Package Versions with renv

Currently there is no package version lock, so the pipeline could behave
differently on different machines as packages are updated.

**To add `renv`:**

```r
install.packages("renv")
renv::init()       # creates renv.lock — commit this file
renv::snapshot()   # update lock file after adding packages
```

**To restore on a new machine:**

```r
renv::restore()    # installs exact versions from renv.lock
```

Commit `renv.lock` to git. The `renv/library/` cache is already in `.gitignore`.

---

## Summary Table

| Option | Who can use it | Requires R install | Writes to Sheets | Cost |
|--------|---------------|--------------------|-----------------|------|
| Manual share | Anyone with link | No | Yes (manual) | Free |
| shinyapps.io | Anyone with URL | No | No (view only) | Free / ~$9/mo |
| GitHub Actions | Anyone with repo access | No | Optional | Free (2000 min/mo) |
| Quarto Pub | Anyone with URL | No | No (read-only) | Free |
| Docker | Technical users | No (Docker yes) | Yes | Free |

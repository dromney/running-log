# Running Log Automation

This repository contains an automated R script that processes a running log from Google Sheets, runs a Generalized Additive Model (GAM) to predict race paces, and generates visualizations and markdown summaries. 

The script is executed automatically via GitHub Actions and pushes updated figures and text files directly back to a specified Google Drive folder for web embedding and NotebookLM syncing.

## Architecture & Workflow
1. **Data Source:** A Google Sheet containing the raw running log.
2. **Runner:** A GitHub Actions Ubuntu environment spins up on a scheduled cron job.
3. **Processing:** The R script runs headless, utilizing the `mgcv`, `modelsummary`, and `ggplot2` packages.
4. **Storage Destination:** Output files (`.png`, `.md`, `.docx`) are uploaded to a Google Drive folder via the `googledrive` package.

## Setup Instructions

To recreate or maintain this automated pipeline, follow these exact steps to configure Google Cloud and GitHub.

### 1. Provision a Google Cloud Service Account
GitHub Actions requires a non-interactive "bot" account to access your Google Drive without triggering a browser-based login prompt.

1. Go to the [Google Cloud Console](https://console.cloud.google.com/) and create a new Project.
2. Navigate to **APIs & Services > Library** and enable the **Google Sheets API** and the **Google Drive API**.
3. Go to **IAM & Admin > Service Accounts** and click **Create Service Account**. 
4. Note the newly generated email address for this service account (e.g., `name@project.iam.gserviceaccount.com`).
5. Go to the **Keys** tab for the service account. Click **Add Key > Create new key**, select **JSON**, and download the file.

### 2. Grant Permissions in Google Drive
*Crucial:* Service accounts cannot inherently see your personal files.
1. Open the target **Running Log Google Sheet**. Click "Share" and invite the Service Account's email address with **Editor** permissions.
2. Open the **Google Drive folder** where the output files will be hosted and share it with the Service Account email as an **Editor**.

### 3. Configure GitHub Secrets
1. Ensure this GitHub repository is set to **Private** to protect your underlying data keys.
2. In the repository, navigate to **Settings > Secrets and variables > Actions**.
3. Click **New repository secret**.
4. Name the secret exactly `GOOGLE_AUTH_JSON`.
5. Open the downloaded `.json` key in a text editor, copy all of the contents, and paste them into the value field. Save the secret.

### 4. The "Seed Run" (Bypassing the 0-Byte Quota)
By default, Google Cloud Service Accounts have a 0-byte storage quota. If the bot attempts to create brand-new files in your Drive, it will fail with a `403 Forbidden` error. 
To bypass this, you must "seed" the folder using your personal account:
1. Run the `running.R` script locally on your computer **one time** using your personal interactive Google account authentication.
2. Verify that all output files (`.png`, `.md`, `.docx`) successfully appear in your Google Drive folder.
3. Because *you* uploaded them first, your personal account owns the files. When the GitHub Action runs subsequently, the bot will use `drive_update()` (via `drive_put()`) to quietly overwrite the existing files, successfully bypassing its own storage restrictions.

### 5. GitHub Actions YAML
The automation is handled by the `.github/workflows/update_running_log.yml` file. 

**Key YAML Features:**
* **Cron Schedule:** Configured to run daily. *Note: GitHub Actions operates on UTC time, so the cron hours must be offset for your local timezone (and adjusted manually for Daylight Saving Time).*
* **System Dependencies:** Installs necessary Ubuntu Linux libraries (e.g., `libcurl4-openssl-dev`, `libfontconfig1-dev`) required for R packages to render headless HTML and visualizations.
* **Package Caching:** Caches the R library to dramatically speed up consecutive runs by skipping full package re-installations. 
* **Auth Generation:** Temporarily writes the `GOOGLE_AUTH_JSON` secret to a local file for the R script to use, then destroys the environment securely upon completion. 
-- Optional signup profile fields -- mirrors the closed-beta waitlist form
-- on the marketing site (landing/index.html's #beta form) so signups
-- through the real app capture the same information the business already
-- collects, without requiring any of it (unlike the waitlist form, none
-- of these are required here -- account creation only needs
-- practice_name/email/password, see directCareAuth::practice_create()).
--
-- All nullable; the app always writes a trimmed string (possibly empty),
-- same convention as the existing `address` column (002), not NULL, but
-- nullable here too so a direct SQL insert/older row without these columns
-- degrades to NULL rather than erroring.

ALTER TABLE practices
    ADD COLUMN first_name           TEXT,
    ADD COLUMN last_name            TEXT,
    -- "Physician" / "Nurse Practitioner" / "Mental Health Therapist" /
    -- "Other" -- same options as the waitlist form's Practice Type
    -- dropdown; free text when "Other" goes in practice_type_other.
    ADD COLUMN practice_type        TEXT,
    ADD COLUMN practice_type_other  TEXT,
    -- Same four options as the waitlist form's Practice Status dropdown
    -- ("Just exploring" / "Planning to launch a direct care practice" /
    -- "Direct care practice launched within the last year" / "Direct
    -- care practice launched more than a year ago").
    ADD COLUMN practice_status      TEXT,
    ADD COLUMN practice_specialty   TEXT,
    -- "How did you hear about us?" -- free text, matches the waitlist form.
    ADD COLUMN referral_source      TEXT;

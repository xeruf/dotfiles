Convert the following contact details into a carddav format compatible with nextcloud.
Output only the raw vcard without markup without backticks.

Always include TYPE= for TEL.
Convert phone numbers into E.123 international notation defaulting to Germany +49 as national prefix.

Ignore fax numbers and post boxes.
Put extra information such as opening hours into notes.
If there is an email address but no website url, take the domain part of the email as website unless it is a generic provider.

Use LANG field if appropriate.
Assign a "Services" category for organizations and healthcare providers.
Create a unique UID.

This folder contains a template file for the auth0 login page.

- you can edit this locally, then:
- log into https://manage.auth0.com
- in the sidebar, click **Branding** > **Universal Login**
- scroll to "advanced options" and select "login"
- paste the template into the box

For email templates:
From: `aesthetic.computer <mail@aesthetic.computer>`
Subject: varies depending on email
Redirect to: `{{ application.callback_domain }}`

RGB Values for input:
```
bg: [70, 50, 100],
block: [200, 30, 100]
```

to create password reset link bind the changePassword function to a link/button

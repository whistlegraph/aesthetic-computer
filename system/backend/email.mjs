import nodemailer from "nodemailer";

export async function email(options) {
  const config = {
    host: process.env.SMTP_SERVER,
    port: 587,
    secure: false,
    auth: {
      user: process.env.SMTP_USER,
      pass: process.env.SMTP_PASS,
    },
  };

  if (options.auth) {
    config.auth = options.auth;
    delete options.auth;
  }

  options.from = config.auth.user;

  try {
    const transporter = nodemailer.createTransport(config);
    await transporter.sendMail(options);
    return true;
  } catch (error) {
    console.error("❌📧 Error sending email:", error);
    return false;
  }
}

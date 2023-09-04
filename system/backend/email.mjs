import nodemailer from "nodemailer";

const config = {
  host: process.env.SMTP_SERVER,
  port: 587,
  secure: false,
  auth: {
    user: process.env.SMTP_USER,
    pass: process.env.SMTP_PASS,
  },
};

export async function email(options) {
  options.from = process.env.SMTP_USER;
  try {
    const transporter = nodemailer.createTransport(config);
    await transporter.sendMail(options);
    return true;
  } catch (error) {
    console.error("Error sending email:", error);
    return false;
  }
}

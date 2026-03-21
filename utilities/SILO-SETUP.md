# Silo MongoDB Authentication Setup Guide

This guide walks you through setting up secure MongoDB authentication on silo.aesthetic.computer and storing the credentials in your vault.

## 🎯 Goal

Create a secure, authenticated MongoDB connection string for silo.aesthetic.computer and store it in `aesthetic-computer-vault/silo/.env`.

---

## 📋 Prerequisites

- SSH access to silo.aesthetic.computer
- sudo/root privileges on silo
- MongoDB installed on silo (Community Edition)

---

## 🚀 Quick Setup (3 Steps)

### Step 1: Copy Setup Script to Silo

```bash
# From your local machine
scp utilities/setup-silo-mongodb-auth.sh root@silo.aesthetic.computer:/tmp/
```

**Note:** Replace `root` with your actual username if different (try: `ubuntu`, `admin`, etc.)

### Step 2: Run Setup Script on Silo

```bash
# SSH into silo
ssh root@silo.aesthetic.computer

# Run the setup script
sudo bash /tmp/setup-silo-mongodb-auth.sh
```

The script will:
- ✅ Generate two secure random passwords (32 characters each)
- ✅ Create MongoDB admin user (full root access)
- ✅ Create application user (read/write on 'aesthetic' database)
- ✅ Enable authentication in `/etc/mongod.conf`
- ✅ Ask if you want to allow remote connections or keep localhost-only
- ✅ Restart MongoDB with new settings
- ✅ Test authentication
- ✅ Save credentials to `/root/mongodb-credentials.txt`

**Interactive Prompt:**
```
Network Configuration:
1. Keep MongoDB on localhost only (SSH tunnel required) - MOST SECURE
2. Bind to 0.0.0.0 to allow remote connections - REQUIRES FIREWALL SETUP

Choose option [1/2] (default: 1):
```

**Recommendation:** Choose **1** (localhost-only) for maximum security. You can always change this later.

### Step 3: Copy Credentials to Vault

```bash
# From your local machine
./utilities/copy-silo-credentials.sh root@silo.aesthetic.computer
```

This will:
- Download `/root/mongodb-credentials.txt` from silo
- Extract the passwords
- Update `aesthetic-computer-vault/silo/.env` with real credentials
- Display the connection string

---

## 🔐 Generated Credentials

After running the setup, you'll have:

### Admin User (Root Access)
- **Username:** `admin`
- **Password:** (auto-generated, 32 chars)
- **Database:** `admin`
- **Roles:** `root`
- **Use for:** Database administration, backups, user management

### Application User (Day-to-Day Use)
- **Username:** `aesthetic_app`
- **Password:** (auto-generated, 32 chars)
- **Database:** `aesthetic`
- **Roles:** `readWrite`, `dbAdmin`
- **Use for:** Application connections, API servers, benchmarks

---

## 📝 Connection Strings

### Option A: SSH Tunnel (Localhost-Only MongoDB)

**Recommended for maximum security**

```bash
# 1. Create SSH tunnel (run this on your local machine)
ssh -L 27018:localhost:27017 root@silo.aesthetic.computer

# 2. In another terminal, use this connection string:
export MONGODB_CONNECTION_STRING="mongodb://aesthetic_app:PASSWORD@localhost:27018/aesthetic?authSource=aesthetic"
export MONGODB_NAME="aesthetic"
```

### Option B: Direct Connection (If MongoDB Binds to 0.0.0.0)

**Only if you chose option 2 during setup**

```bash
export MONGODB_CONNECTION_STRING="mongodb://aesthetic_app:PASSWORD@silo.aesthetic.computer:27017/aesthetic?authSource=aesthetic"
export MONGODB_NAME="aesthetic"
```

**Important:** If using direct connection, configure firewall:
```bash
# On silo server
sudo ufw allow from YOUR_IP_ADDRESS to any port 27017
```

---

## 🧪 Testing the Connection

### Test 1: From Silo Server (Always Works)

```bash
ssh root@silo.aesthetic.computer

mongosh "mongodb://aesthetic_app:PASSWORD@localhost:27017/aesthetic?authSource=aesthetic" \
  --eval "db.runCommand({ping: 1})"
```

### Test 2: Via SSH Tunnel

```bash
# Terminal 1: Create tunnel
ssh -L 27018:localhost:27017 root@silo.aesthetic.computer

# Terminal 2: Test connection
mongosh "mongodb://aesthetic_app:PASSWORD@localhost:27018/aesthetic?authSource=aesthetic" \
  --eval "db.runCommand({ping: 1})"
```

### Test 3: Run Benchmark

```bash
# Set environment variables
source utilities/benchmark-setup-env.sh

# Or manually:
export SILO_MONGODB_CONNECTION_STRING="mongodb://aesthetic_app:PASSWORD@localhost:27018/aesthetic?authSource=aesthetic"
export SILO_MONGODB_NAME="aesthetic"

# Run benchmark
node utilities/benchmark-mongodb.mjs --writes 100 --reads 100
```

---

## 📁 Where Credentials Are Stored

### On Silo Server
- **Location:** `/root/mongodb-credentials.txt`
- **Permissions:** `600` (owner read/write only)
- **Contains:** Both admin and application credentials, connection strings, examples

### In Your Vault
- **Location:** `aesthetic-computer-vault/silo/.env`
- **Format:** Environment variable format for easy sourcing
- **Usage:** `export $(grep -v '^#' aesthetic-computer-vault/silo/.env | xargs)`

---

## 🔄 Updating the Benchmark Script

To use the silo credentials in the benchmark, update the connection string:

```bash
# In utilities/benchmark-mongodb.mjs, update the silo config:
silo: {
  name: "silo.aesthetic.computer",
  connectionString: process.env.SILO_MONGODB_CONNECTION_STRING ||
    "mongodb://aesthetic_app:PASSWORD@localhost:27018/aesthetic?authSource=aesthetic",
  dbName: process.env.SILO_MONGODB_NAME || "aesthetic",
}
```

---

## 🛡️ Security Best Practices

### ✅ DO:
- Keep MongoDB bound to localhost (use SSH tunnel)
- Use strong, auto-generated passwords
- Store credentials in vault (git-ignored)
- Use application user for day-to-day operations
- Use admin user only for administration tasks
- Keep firewall enabled and configured
- Rotate passwords periodically

### ❌ DON'T:
- Commit credentials to git
- Use weak or predictable passwords
- Bind MongoDB to 0.0.0.0 without firewall rules
- Share admin credentials with applications
- Disable authentication after enabling it
- Expose port 27017 to the entire internet

---

## 🔧 Troubleshooting

### "Authentication failed"
- Verify password is correct (check `/root/mongodb-credentials.txt` on silo)
- Ensure you're using `authSource=aesthetic` in connection string
- Check MongoDB logs: `sudo journalctl -u mongod -f`

### "Connection timeout"
- If using localhost: Check SSH tunnel is active (`ps aux | grep ssh`)
- If using direct: Check firewall allows your IP (`sudo ufw status`)
- Verify MongoDB is running: `sudo systemctl status mongod`

### "Connection refused"
- MongoDB might not be running: `sudo systemctl start mongod`
- Wrong port: Default is 27017 (or 27018 for SSH tunnel)
- Check MongoDB is listening: `sudo netstat -tlnp | grep 27017`

### "Operation not permitted"
- User might not have correct roles
- Try with admin user to verify authentication works
- Recreate user if needed (re-run setup script)

---

## 📊 Next Steps

After setup is complete:

1. **Run the benchmark** to compare Atlas vs Silo performance
2. **Update all .env files** to use new silo connection (when ready to migrate)
3. **Set up automated backups** (mongodump to DigitalOcean Spaces)
4. **Test all services** with silo before switching from Atlas

---

## 🆘 Manual Recovery

If something goes wrong, you can manually reset:

```bash
# SSH into silo
ssh root@silo.aesthetic.computer

# Stop MongoDB
sudo systemctl stop mongod

# Restore backup config
sudo cp /etc/mongod.conf.backup.TIMESTAMP /etc/mongod.conf

# Start without auth
sudo mongod --dbpath /var/lib/mongo --noauth --fork --logpath /tmp/mongo-noauth.log

# Drop users and try again
mongosh --eval "
  db.getSiblingDB('admin').dropUser('admin');
  db.getSiblingDB('aesthetic').dropUser('aesthetic_app');
"

# Stop temp mongod
pkill mongod

# Start normal mongod
sudo systemctl start mongod
```

---

## 📚 Additional Resources

- [MongoDB Security Checklist](https://www.mongodb.com/docs/manual/administration/security-checklist/)
- [MongoDB Authentication](https://www.mongodb.com/docs/manual/core/authentication/)
- [SSH Tunneling Guide](https://www.ssh.com/academy/ssh/tunneling/example)

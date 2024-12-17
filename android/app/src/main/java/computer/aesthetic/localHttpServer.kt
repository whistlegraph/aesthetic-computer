package computer.aesthetic

import android.content.Context
import fi.iki.elonen.NanoHTTPD
import java.io.InputStream
import java.security.KeyStore
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.SSLServerSocketFactory

class LocalHttpServer(
    private val context: Context,
    port: Int = 8443 // HTTPS port
) : NanoHTTPD(port) {

    init {
        // Initialize SSL
        makeSecure(loadSSLServerSocketFactory(), null)
    }

    private fun loadSSLServerSocketFactory(): SSLServerSocketFactory {
        val keyStore = KeyStore.getInstance("BKS") // Use BKS or JKS format
        val keystoreStream: InputStream = context.resources.openRawResource(R.raw.keystore) // Your keystore
        val keystorePassword = "password".toCharArray()

        // Load the keystore
        keyStore.load(keystoreStream, keystorePassword)

        // Initialize KeyManagerFactory
        val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm())
        keyManagerFactory.init(keyStore, keystorePassword)

        // Set up SSLContext
        val sslContext = SSLContext.getInstance("TLS")
        sslContext.init(keyManagerFactory.keyManagers, null, null)

        return sslContext.serverSocketFactory
    }

    override fun serve(session: IHTTPSession?): Response {
        val uri = session?.uri ?: "/"
        val filePath = if (uri == "/") "index.html" else uri.trimStart('/')

        return try {
            val inputStream = context.assets.open(filePath)
            val content = inputStream.bufferedReader().use { it.readText() }
            newFixedLengthResponse(Response.Status.OK, "text/html", content)
        } catch (e: Exception) {
            newFixedLengthResponse(Response.Status.NOT_FOUND, "text/plain", "404 Not Found")
        }
    }
}

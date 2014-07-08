package fr.putnami.pwt.plugin.spring.rpc.server.util;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

public final class RequestThreadLocalUtils {

	public static final ThreadLocal<HttpServletRequest> TL_REQUEST = new ThreadLocal<HttpServletRequest>();
	public static final ThreadLocal<HttpServletResponse> TL_RESPONSE = new ThreadLocal<HttpServletResponse>();
	public static final ThreadLocal<HttpSession> TL_SESSION = new ThreadLocal<HttpSession>();

	private RequestThreadLocalUtils() {
	}

	public static HttpServletRequest getRequest() {
		return TL_REQUEST.get();
	}

	public static HttpServletResponse getResponse() {
		return TL_RESPONSE.get();
	}

	public static HttpSession getSession() {
		return TL_SESSION.get();
	}

	public static void resetContext() {
		TL_REQUEST.set(null);
		TL_RESPONSE.set(null);
		TL_SESSION.set(null);
	}

	public static void initContext(HttpServletRequest req, HttpServletResponse resp) {
		TL_REQUEST.set(req);
		TL_RESPONSE.set(resp);
		TL_SESSION.set(req.getSession());
	}
}

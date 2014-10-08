/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
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
		return RequestThreadLocalUtils.TL_REQUEST.get();
	}

	public static HttpServletResponse getResponse() {
		return RequestThreadLocalUtils.TL_RESPONSE.get();
	}

	public static HttpSession getSession() {
		return RequestThreadLocalUtils.TL_SESSION.get();
	}

	public static void resetContext() {
		RequestThreadLocalUtils.TL_REQUEST.set(null);
		RequestThreadLocalUtils.TL_RESPONSE.set(null);
		RequestThreadLocalUtils.TL_SESSION.set(null);
	}

	public static void initContext(HttpServletRequest req, HttpServletResponse resp) {
		RequestThreadLocalUtils.TL_REQUEST.set(req);
		RequestThreadLocalUtils.TL_RESPONSE.set(resp);
		RequestThreadLocalUtils.TL_SESSION.set(req.getSession());
	}
}

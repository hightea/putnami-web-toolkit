/**
 * This file is part of pwt-ajax-bot-indexing.
 *
 * pwt-ajax-bot-indexing is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-ajax-bot-indexing is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-ajax-bot-indexing.  If not, see <http://www.gnu.org/licenses/>.
 */
/**
 * This file is part of pwt. pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pwt is distributed in the
 * hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details. You should have received a copy of the GNU Lesser General Public License along with
 * pwt. If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.ajaxbot;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.tools.ant.filters.StringInputStream;

import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.WebRequest;
import com.gargoylesoftware.htmlunit.WebResponse;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.DomNode;
import com.gargoylesoftware.htmlunit.html.HtmlInlineFrame;
import com.gargoylesoftware.htmlunit.html.HtmlItalic;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlScript;
import com.gargoylesoftware.htmlunit.util.FalsifyingWebConnection;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.gwt.thirdparty.guava.common.io.ByteStreams;

public class AjaxBotIndexingFilter implements Filter {


	private static final String FILTER_PARAM_CACHE_RESET_ON_STARTUP = "cacheResetOnStartup";
	private static final String FILTER_PARAM_CACHE_FOLDER = "cacheFolder";

	private static final String QUERY_PARAM_ESCAPED_FRAGMENT = "_escaped_fragment_";
	private static final String QUERY_PARAM_RESET_FILTER = "_ajaxbotfilter_cache_reset_";

	private File cacheFolder;

	@Override
	public void init(FilterConfig config) throws ServletException {
		String paramCacheFolder = config.getInitParameter(FILTER_PARAM_CACHE_FOLDER);
		if (!Strings.isNullOrEmpty(paramCacheFolder)) {
			cacheFolder = new File(paramCacheFolder);
		}
		if (Boolean.valueOf(config.getInitParameter(FILTER_PARAM_CACHE_RESET_ON_STARTUP))) {
			resetCache();
		}
	}

	@Override
	public void destroy() {
	}

	@Override
	public void doFilter(ServletRequest req, ServletResponse resp, FilterChain chain) throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) req;
		String token = request.getParameter(QUERY_PARAM_ESCAPED_FRAGMENT);
		if ("GET".equals(request.getMethod())
				&& request.getParameter(QUERY_PARAM_RESET_FILTER) != null) {
			resetCache();
		}
		if ("GET".equals(request.getMethod()) && token != null) {
			ByteStreams.copy(getHtmlStream(request), resp.getOutputStream());
		}
		else {
			chain.doFilter(req, resp);
		}
	}

	private InputStream getHtmlStream(HttpServletRequest request) throws IOException {

		String cacheFileName = getCacheFileName(request);
		if (cacheFileName == null) {
			return extractHtml(request);
		}
		File cacheFile = new File(cacheFileName);
		if (cacheFile.exists() && cacheFile.isFile()) {
			return new FileInputStream(cacheFile);
		}
		else {
			return extractHtml(request);
		}
	}

	private InputStream extractHtml(HttpServletRequest request) throws IOException {
		WebClient webClient = new WebClient(BrowserVersion.CHROME);
		new GoogleAnalyticsConnectionFilter(webClient);

		String token = request.getParameter(QUERY_PARAM_ESCAPED_FRAGMENT);
		String escapedTokenQuery = QUERY_PARAM_ESCAPED_FRAGMENT + "=" + token;

		String uri = request.getRequestURI();
		String host = request.getServerName();
		int port = request.getServerPort();
		String query = request.getQueryString();

		if (token.contains("\\&" + escapedTokenQuery)) {
			query = query.replace("&" + escapedTokenQuery, "");
		}
		else if (token.contains(escapedTokenQuery + "&")) {
			query = query.replace("&" + escapedTokenQuery, "");
		}
		else {
		}
		query = query.replace(escapedTokenQuery, "");
		if (query.contains("&&")) {
			query = query.replace("&&", "");
		}
		else if (query.endsWith("&")) {
			query = query.substring(0, query.length() - 1);
		}

		StringBuffer prettyUrl = new StringBuffer();
		prettyUrl.append(request.isSecure() ? "https://" : "http://")
		.append(host)
		.append(":")
		.append(port)
		.append(uri);

		if (query.length() > 1) {
			prettyUrl.append("?")
			.append(query);
		}

		prettyUrl.append("#!")
		.append(token);

		HtmlPage page = webClient.getPage(prettyUrl.toString());
		List<DomNode> elementsToRemove = Lists.newArrayList();

		// GWT iframe are useless
		for (DomNode node : page.getBody().getChildren()) {
			if (HtmlInlineFrame.TAG_NAME.equals(node.getNodeName())) {
				// Remove the iframe
				elementsToRemove.add(node);
			}
		}

		// remove all javascript from the header, the bot will not try process them.
		for (DomElement elem : page.getHead().getElementsByTagName(HtmlScript.TAG_NAME)) {
			HtmlScript scriptElem = (HtmlScript) elem;
			if (scriptElem.getAttribute("src") != null) {
				elementsToRemove.add(scriptElem);
			}
		}
		// remove the i (icon) tags.
		elementsToRemove.addAll(page.getBody().getElementsByTagName(HtmlItalic.TAG_NAME));

		// Remove elements from dom
		for (DomNode elem : elementsToRemove) {
			elem.remove();
		}

		// Reset the html doctype
		String pageData = page.asXml().replace("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<!DOCTYPE html>");

		String cacheFileName = getCacheFileName(request);
		if (cacheFileName != null) {
			File cacheFile = new File(cacheFileName);
			cacheFile.getParentFile().mkdirs();
			PrintWriter out = new PrintWriter(cacheFile);
			out.print(pageData);
			out.close();

			return new FileInputStream(cacheFile);
		}
		else {
			return new StringInputStream(pageData);
		}
	}

	private void resetCache() {
		deleteFolder(cacheFolder);
	}

	public void deleteFolder(File folder) {
		if (folder == null) {
			return;
		}
		File[] files = folder.listFiles();
		if (files != null) { // some JVMs return null for empty dirs
			for (File f : files) {
				if (f.isDirectory()) {
					deleteFolder(f);
				}
				else {
					f.delete();
				}
			}
		}
		folder.delete();
	}
	private String getCacheFileName(HttpServletRequest request) {
		if (cacheFolder == null) {
			return null;
		}
		String cacheFileName = cacheFolder.getAbsolutePath() + request.getRequestURI();
		String token = request.getParameter(QUERY_PARAM_ESCAPED_FRAGMENT);
		if (token != null) {
			cacheFileName += token;
		}
		cacheFileName += ".html";
		return cacheFileName;
	}

	class GoogleAnalyticsConnectionFilter extends FalsifyingWebConnection {
		public GoogleAnalyticsConnectionFilter(WebClient webClient) throws IllegalArgumentException {
			super(webClient);
		}

		@Override
		public WebResponse getResponse(WebRequest request) throws IOException {
			WebResponse response = super.getResponse(request);
			if (response.getWebRequest().getUrl().toString().contains("www.google-analytics.com")) {
				return createWebResponse(response.getWebRequest(), "", "application/javascript", 200, "Ok");
			}
			return super.getResponse(request);
		}
	}
}

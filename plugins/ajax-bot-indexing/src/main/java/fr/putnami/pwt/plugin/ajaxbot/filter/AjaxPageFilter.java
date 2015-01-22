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
package fr.putnami.pwt.plugin.ajaxbot.filter;

import com.google.common.base.Strings;
import com.google.gwt.thirdparty.guava.common.io.ByteStreams;

import org.apache.tools.ant.filters.StringInputStream;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpMethod;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.annotation.PostConstruct;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import static fr.putnami.pwt.plugin.ajaxbot.util.AjaxBotUtils.FILTER_PARAM_CACHE_FOLDER;
import static fr.putnami.pwt.plugin.ajaxbot.util.AjaxBotUtils.FILTER_PARAM_SERVER_URL;
import static fr.putnami.pwt.plugin.ajaxbot.util.AjaxBotUtils.QUERY_PARAM_ESCAPED_FRAGMENT;

import fr.putnami.pwt.plugin.ajaxbot.helper.AjaxPageRenderer;

public class AjaxPageFilter implements Filter {

	@Value("${ajaxbotfilter.cachefolder}")
	private File cacheFolder;
	@Value("${ajaxbotfilter.server.url}")
	private String serverUrl;

	@PostConstruct
	public void postConstruct() {
		this.cacheFolder.mkdirs();
	}

	@Override
	public void init(FilterConfig config) throws ServletException {
		String paramCacheFolder =
			config.getInitParameter(FILTER_PARAM_CACHE_FOLDER);
		if (!Strings.isNullOrEmpty(paramCacheFolder)) {
			this.cacheFolder = new File(paramCacheFolder);
			this.cacheFolder.mkdirs();
		}
		String serverUrl =
			config.getInitParameter(FILTER_PARAM_SERVER_URL);
		if (!Strings.isNullOrEmpty(paramCacheFolder)) {
			this.serverUrl = serverUrl;
		}
	}

	@Override
	public void destroy() {
	}

	private static String rewriteQueryString(String queryString) throws UnsupportedEncodingException {
		StringBuilder queryStringSb = new StringBuilder(queryString);
		int i = queryStringSb.indexOf("&" + QUERY_PARAM_ESCAPED_FRAGMENT);
		if (i != -1) {
			StringBuilder tmpSb = new StringBuilder(queryStringSb.substring(0, i));
			tmpSb.append("#!");
			tmpSb.append(URLDecoder.decode(queryStringSb.substring(i + 20, queryStringSb.length()),
				"UTF-8"));
			queryStringSb = tmpSb;
		}

		i = queryStringSb.indexOf(QUERY_PARAM_ESCAPED_FRAGMENT);
		if (i != -1) {
			StringBuilder tmpSb = new StringBuilder(queryStringSb.substring(0, i));
			tmpSb.append("#!");
			tmpSb.append(URLDecoder.decode(queryStringSb.substring(i + 19, queryStringSb.length()),
				"UTF-8"));
			queryStringSb = tmpSb;
		}
		if (queryStringSb.indexOf("#!") != 0) {
			queryStringSb.insert(0, '?');
		}

		return queryStringSb.toString();
	}

	@Override
	public void doFilter(ServletRequest req, ServletResponse resp, FilterChain chain)
		throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) req;

		if (HttpMethod.GET.toString().equals(request.getMethod())
			&& req.getParameter(QUERY_PARAM_ESCAPED_FRAGMENT) != null) {
			ByteStreams.copy(this.getHtmlStream(request), resp.getOutputStream());
		} else {
			chain.doFilter(req, resp);
		}
	}

	private InputStream getHtmlStream(HttpServletRequest request) throws IOException {
		String cacheFileName = this.getCacheFileName(request.getParameter(QUERY_PARAM_ESCAPED_FRAGMENT));
		if (cacheFileName == null) {
			return this.extractHtml(request);
		}
		File cacheFile = new File(cacheFileName);
		if (cacheFile.exists() && cacheFile.isFile()) {
			return new FileInputStream(cacheFile);
		}
		return this.extractHtml(request);
	}

	private InputStream extractHtml(HttpServletRequest request) throws IOException {
		AjaxPageRenderer crowler = new AjaxPageRenderer(serverUrl);
		String pageData = crowler.crawlPage(AjaxPageFilter.rewriteQueryString(request.getQueryString()));
		String cacheFileName = this.getCacheFileName(request.getParameter(QUERY_PARAM_ESCAPED_FRAGMENT));
		if (cacheFileName != null) {
			File cacheFile = new File(cacheFileName);
			cacheFile.getParentFile().mkdirs();
			PrintWriter out = new PrintWriter(cacheFile);
			out.print(pageData);
			out.close();

			return new FileInputStream(cacheFile);
		}
		return new StringInputStream(pageData);
	}

	private String getCacheFileName(String token) {
		if (this.cacheFolder == null) {
			return null;
		}
		return this.cacheFolder.getAbsolutePath() + "/" + token + ".html";
	}
}

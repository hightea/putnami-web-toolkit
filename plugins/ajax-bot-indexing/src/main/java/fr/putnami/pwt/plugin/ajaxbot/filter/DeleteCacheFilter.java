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

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpMethod;

import java.io.File;
import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import fr.putnami.pwt.plugin.ajaxbot.util.AjaxBotUtils;

public class DeleteCacheFilter implements Filter {

	private static final String FILTER_PARAM_CACHE_FOLDER = "cacheFolder";

	private static final String QUERY_PARAM_RESET_FILTER = "_ajaxbotfilter_cache_reset_";

	@Value("${ajaxbotfilter.cachefolder}")
	private File cacheFolder;

	@Override
	public void init(FilterConfig config) throws ServletException {
		String paramCacheFolder =
			config.getInitParameter(DeleteCacheFilter.FILTER_PARAM_CACHE_FOLDER);
		if (!Strings.isNullOrEmpty(paramCacheFolder)) {
			this.cacheFolder = new File(paramCacheFolder);
			this.cacheFolder.mkdirs();
		}
	}

	@Override
	public void destroy() {
	}

	@Override
	public void doFilter(ServletRequest req, ServletResponse resp, FilterChain chain)
		throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) req;
		if (HttpMethod.GET.toString().equals(request.getMethod())
			&& null != request.getParameter(QUERY_PARAM_RESET_FILTER)) {
			this.resetCache();
		}
		chain.doFilter(req, resp);
	}

	private void resetCache() {
		AjaxBotUtils.deleteFolder(this.cacheFolder);
	}
}

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
package fr.putnami.pwt.plugin.ajaxbot.helper;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Set;

public class SiteCrowler {

	private final AjaxPageRenderer crowler;
	private final Set<String> pages = Sets.newHashSet();
	private String folderCache;
	private String baseUrl;

	public SiteCrowler(String baseUrl, String folderCache) {
		this.baseUrl = baseUrl;
		this.folderCache = folderCache;
		crowler = new AjaxPageRenderer(baseUrl);
	}

	private void writeSiteMap(String sitemap) throws IOException {
		PrintWriter out = new PrintWriter(sitemap);
		for (String page : pages) {
			out.println(baseUrl + page);
		}
		out.close();
	}
	private void crowlPage(String page) throws IOException {
		String pageData = crowler.crawlPage(page);
		String cacheFileName = folderCache + page.replaceFirst("#!", "") + ".html";
		File cacheFile = new File(cacheFileName);
		cacheFile.getParentFile().mkdirs();
		PrintWriter out = new PrintWriter(cacheFile);
		out.print(pageData);
		out.close();
		List<String> pageToParse = Lists.newArrayList();
		for (String nextPage : crowler.getAnchors()) {
			if (!pages.contains(nextPage)) {
				pages.add(nextPage);
				pageToParse.add(nextPage);
			}
		}
		for (String nextPage : pageToParse) {
			crowlPage(nextPage);
		}
	}

	public static void main(String[] args) {
		if (args.length != 3) {
			throw new RuntimeException("Wrong argument");
		}
		SiteCrowler crowler = new SiteCrowler(args[0], args[1]);
		try {
			crowler.crowlPage("#!");
			crowler.writeSiteMap(args[2]);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}

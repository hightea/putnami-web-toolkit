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
package fr.putnami.pwt.plugin.ajaxbot.controller;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.servlet.http.HttpServletResponse;

import fr.putnami.pwt.plugin.ajaxbot.util.AjaxBotUtils;

@Controller
public class SiteMapController {

	@Value("${ajaxbotfilter.sitemap}")
	private File sitemap;
	@Value("${ajaxbotfilter.cachefolder}")
	private File cacheFolder;

	@RequestMapping(value = "/sitemap.txt", method = RequestMethod.GET)
	public void welcomePage(HttpServletResponse response) {
		try {
			InputStream is = new FileInputStream(sitemap);
			response.setContentType("text/plain");
			response.setContentLength((int) sitemap.length());
			IOUtils.copy(is, response.getOutputStream());
			response.flushBuffer();
		} catch (IOException ex) {
			throw new RuntimeException("IOError writing file to output stream", ex);
		}
	}

	@RequestMapping(value = "/seo/deleteAjaxCache", method = RequestMethod.GET)
	public void deleteAjaxCache() {
		AjaxBotUtils.deleteFolder(this.cacheFolder);
	}

}

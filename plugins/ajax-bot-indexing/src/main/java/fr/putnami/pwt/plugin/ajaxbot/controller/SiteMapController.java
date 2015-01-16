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

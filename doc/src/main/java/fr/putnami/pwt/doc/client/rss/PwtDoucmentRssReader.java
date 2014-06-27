/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.rss;

import java.util.Date;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.Cookies;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.xml.client.DOMException;
import com.google.gwt.xml.client.Document;
import com.google.gwt.xml.client.Element;
import com.google.gwt.xml.client.NodeList;
import com.google.gwt.xml.client.XMLParser;

public class PwtDoucmentRssReader {

	private final DateTimeFormat dateFormatter = DateTimeFormat.getFormat("EEE, d MMM yyyy");

	private final String rssUrl;

	public PwtDoucmentRssReader(String rssUrl) {
		this.rssUrl = rssUrl;
	}

	public void load(final AsyncCallback<List<RssItem>> callback) {
		RequestCallback requestCallback = new RequestCallback() {

			@Override
			public void onResponseReceived(Request request, Response response) {
				List<RssItem> items = Lists.newArrayList();
				try {
					Document messageDom = XMLParser.parse(response.getText());

					NodeList nodes = messageDom.getElementsByTagName("item");
					for (int i = 0; i < nodes.getLength() && items.size() < 10; i++) {
						Element item = (Element) nodes.item(i);
						RssItem rssItem = new RssItem();
						rssItem.setTitle(getStringValue(item, "title"));
						rssItem.setDescription(getStringValue(item, "description"));
						rssItem.setLink(getStringValue(item, "link"));
						rssItem.setGuid(getStringValue(item, "guid"));
						rssItem.setAuthor(getStringValue(item, "author"));
						rssItem.setPubDate(getDateValue(item, "pubDate"));

						boolean toAdd = true;

						if (Cookies.isCookieEnabled()) {
							String cookie = Cookies.getCookie(getCookieName(rssItem.getGuid()));
							if (cookie != null && cookie.length() > 0) {
								toAdd = false;
							}
						}
						if(toAdd){
							items.add(rssItem);
						}
					}

					callback.onSuccess(items);
				}
				catch (DOMException e) {
					callback.onFailure(e);
				}
			}

			@Override
			public void onError(Request request, Throwable exception) {
				callback.onFailure(exception);
			}
		};
		RequestBuilder builder = new RequestBuilder(RequestBuilder.GET, this.rssUrl);
		builder.setCallback(requestCallback);
		try {
			builder.send();
		}
		catch (RequestException e) {
			callback.onFailure(e);
		}

	}

	private Date getDateValue(Element item, String tag) {
		String text = getStringValue(item, tag);
		if (text != null) {
			try {
				return dateFormatter.parse(text);
			}
			catch (IllegalArgumentException e) {
				return null;
			}
		}
		return null;
	}

	private String getStringValue(Element item, String tag) {
		String result = null;
		NodeList elements = item.getElementsByTagName(tag);
		if (elements.getLength() > 0) {
			result = elements.item(0).getFirstChild().getNodeValue();
		}
		return result;
	}

	private String getCookieName(String guid) {
		return "PWT_READ_NEWS:" + guid;
	}

	public void read(RssItem value) {
		if(Cookies.isCookieEnabled()){
			Cookies.setCookie(getCookieName(value.getGuid()), new Date() + "");
		}
	}
}

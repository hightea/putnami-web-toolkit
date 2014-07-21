/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.application;

import java.util.Collections;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.service.client.CallbackAdapter;
import fr.putnami.pwt.core.widget.client.Alert;
import fr.putnami.pwt.core.widget.client.Anchor;
import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.OutputList;
import fr.putnami.pwt.core.widget.client.OutputNumber;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.AlertDismissEvent;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.doc.client.rss.PwtDoucmentRssReader;
import fr.putnami.pwt.doc.client.rss.RssItem;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;

public class NewsPopup implements IsWidget, HasDrawable {

	interface Binder extends UiBinderLocalized<Widget, NewsPopup> {
		Binder BINDER = GWT.create(Binder.class);
	}

	public interface NotificationModel extends Model<RssItem> {
	}

	private final Model<RssItem> notificationModel = GWT.create(NotificationModel.class);

	@UiField
	Anchor<?> notificationWidget;
	@UiField
	OutputNumber notificationCount;

	@UiField
	Modal notificationModal;

	@UiField
	OutputList<RssItem> notificationList;
	@UiField
	Alert<RssItem> notificationListItem;

	private List<RssItem> notifications;

	private final PwtDoucmentRssReader rssReader = new PwtDoucmentRssReader("news-rss.xml");

	public NewsPopup() {
		Binder.BINDER.createAndBindUi(this);
		notificationList.initialize(notificationModel);
		notificationListItem.initialize(notificationModel);
		reload();
	}

	@Override
	public Widget asWidget() {
		return notificationWidget;
	}

	@UiHandler("notificationWidget")
	void onNotificationWidgetClicked(ClickEvent event) {
		if (notifications != null && notifications.size() > 0) {
			notificationModal.show();
			getGA().trackEvent("popup", "news");
		}
	}

	private void reload() {
		notifications = Lists.newArrayList();

		rssReader.load(new CallbackAdapter<List<RssItem>>() {
			@Override
			public void onSuccess(List<RssItem> result) {
				notifications = result;
				redraw();
			}
		});

		redraw();
	}

	@Override
	public void redraw() {
		if (notifications != null && notifications.size() > 0) {
			notificationList.edit(notifications);
			notificationCount.edit(notifications.size());
			notificationCount.setVisible(true);
		}
		else {
			notificationList.edit(Collections.EMPTY_LIST);
			notificationCount.setVisible(false);
			notificationCount.edit(0);
		}
	}

	@UiHandler("notificationListItem")
	void onNotificationClose(AlertDismissEvent event) {
		Alert<RssItem> closed = (Alert<RssItem>) event.getSource();
		rssReader.read(closed.getValue());
		notifications.remove(closed.getValue());
		redraw();
	}

	@UiHandler("seeMoreBtn")
	void onSeeMoreClick(ButtonEvent event) {
		RssItem item = event.getValue();
		getGA().trackEvent("read", "news" + item.getGuid());
		String link = item.getLink();
		if (link.startsWith("#!")) {
			History.newItem(link.substring(1));
		}
		else {
			Window.open(link, "_blank", "");
		}
	}

	private GoogleAnalytics getGA() {
		return GoogleAnalytics.get(ApplicationConfig.ANALYTICS_TRACKER_ID);
	}
}

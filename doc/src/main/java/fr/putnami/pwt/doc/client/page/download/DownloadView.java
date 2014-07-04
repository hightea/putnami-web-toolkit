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
package fr.putnami.pwt.doc.client.page.download;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.doc.client.application.Page;

public class DownloadView extends Page<DownloadPlace> {

	interface Binder extends UiBinderLocalized<Widget, DownloadView> {

		Binder BINDER = GWT.create(Binder.class);
	}

	@Override
	protected UiBinderLocalized getBinder() {
		return GWT.create(Binder.class);
	}

//	@UiHandler("content")
	//	public void onClick(ClickEvent evt) {
	//		GoogleAnalytics.get(ApplicationConfig.ANALYTICS_TRACKER_ID).trackEvent("download", "pwt-core-jar");
	//	}
}

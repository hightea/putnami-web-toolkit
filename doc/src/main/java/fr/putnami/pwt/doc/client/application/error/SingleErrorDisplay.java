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
package fr.putnami.pwt.doc.client.application.error;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Element;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.safehtml.shared.SafeHtmlUtils;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;

public class SingleErrorDisplay {

	interface Binder extends UiBinderLocalized<Modal, SingleErrorDisplay> {
		Binder BINDER = GWT.create(Binder.class);
	}

	interface Constants extends ConstantsWithLookup {

		@DefaultStringValue("Ooops an error occured !")
		String errorDialogTitle();

		@DefaultStringValue("Reload")
		String reloadButton();
	}

	@UiField(provided = true)
	Constants constants = GWT.create(Constants.class);

	@UiField
	Modal modal;

	@UiField
	Element titleOutput;
	@UiField
	Label messageOutput;

	private boolean showing;

	public SingleErrorDisplay() {
		Binder.BINDER.createAndBindUi(this);
	}

	@UiHandler("reloadButton")
	void onReload(ButtonEvent event) {
		Window.Location.reload();
	}

	public void show(String title, String message) {
		titleOutput.setInnerSafeHtml(SafeHtmlUtils.fromString(title));
		messageOutput.setText(message);
		if (!showing) {
			RootPanel.get().add(modal);
			modal.show();
		}
		showing = true;
	}

}

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
package fr.putnami.pwt.doc.client.page.layout;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;

public class ModalView extends Composite {

	interface Binder extends UiBinderLocalized<Widget, ModalView> {

		Binder BINDER = GWT.create(Binder.class);
	}

	@UiField(provided = true)
	final NavSpy navSpy;

	@UiField
	Modal modalForm;

	@UiField
	Modal modalTitle;

	@UiField
	Modal modalWidgetDismissable;

	@UiField
	Modal modalWidget;

	@UiField
	Modal smallModal;

	@UiField
	Modal largeModal;

	@UiConstructor
	public ModalView(NavSpy navSpy) {
		this.navSpy = navSpy;
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

	@UiHandler("openModalFormBtn")
	public void onOpenModalForm(ButtonEvent event) {
		modalForm.show();
	}

	@UiHandler("openModalTitleBtn")
	public void onOpenModalTitle(ButtonEvent event) {
		modalTitle.show();
	}

	@UiHandler("closeModalTitleBtn")
	public void onCloseModalTitle(ButtonEvent event) {
		modalTitle.hide();
	}

	@UiHandler("openModalWidgetDismissableBtn")
	public void onOpenModalWidgetDismissable(ButtonEvent event) {
		modalWidgetDismissable.show();
	}

	@UiHandler("openModalWidgetBtn")
	public void onOpenModalWidget(ButtonEvent event) {
		modalWidget.show();
	}

	@UiHandler("closeModalWidgetBtn")
	public void onCloseModalWidget(ButtonEvent event) {
		modalWidget.hide();
	}

	@UiHandler("openSmallModalBtn")
	public void onOpenSmallModal(ButtonEvent event) {
		smallModal.show();
	}

	@UiHandler("openLargeModalBtn")
	public void onOpenLargeModal(ButtonEvent event) {
		largeModal.show();
	}
}

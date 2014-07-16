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
package fr.putnami.pwt.doc.client.page.sample.all;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class SamplesView extends Composite implements View {

	interface Binder extends UiBinderLocalized<Widget, SamplesView> {
		Binder BINDER = GWT.create(Binder.class);
	}

	public SamplesView() {
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

	@PresentHandler
	public void present() {
		Document.get().setTitle("PWT - Samples");
	}
}

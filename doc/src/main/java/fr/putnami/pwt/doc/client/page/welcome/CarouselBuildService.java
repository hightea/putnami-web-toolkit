package fr.putnami.pwt.doc.client.page.welcome;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class CarouselBuildService extends Composite {

	interface Binder extends UiBinderLocalized<Widget, CarouselBuildService> {
		Binder BINDER = GWT.create(Binder.class);
	}

	public CarouselBuildService() {
		initWidget(Binder.BINDER.createAndBindUi(this));
	}
}

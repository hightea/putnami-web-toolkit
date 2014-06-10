package fr.putnami.pwt.sample.web.client.application;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.SimplePanel;

import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class SampleDisplay extends Composite implements AcceptsOneWidget {

	interface Binder extends UiBinderLocalized<HTMLPanel, SampleDisplay> {
		Binder BINDER = GWT.create(Binder.class);
	}

	@UiField
	SimplePanel viewContainer;

	public SampleDisplay() {
		initWidget(Binder.BINDER.createAndBindUi(this));
	}

	@Override
	public void setWidget(IsWidget w) {
		if (w == null) {
			return;
		}
		viewContainer.setWidget(w);
	}

}

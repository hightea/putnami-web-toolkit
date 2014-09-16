package fr.putnami.pwt.tutorial.client.application;

import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.OneWidgetPanel;

@Templated
public class PageDecorator extends Composite implements AcceptsOneWidget, View {

	@UiField
	OneWidgetPanel viewContainer;

	@Override
	public void setWidget(IsWidget w) {
		if (w == null) {
			return;
		}
		viewContainer.setWidget(w);
	}
}

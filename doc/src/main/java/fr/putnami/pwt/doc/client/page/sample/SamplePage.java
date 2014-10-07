/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.sample;

import com.google.common.collect.Multimap;
import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.Anchor;
import fr.putnami.pwt.core.widget.client.Header;
import fr.putnami.pwt.core.widget.client.List;
import fr.putnami.pwt.core.widget.client.List.Type;
import fr.putnami.pwt.core.widget.client.ListItem;
import fr.putnami.pwt.core.widget.client.Panel;
import fr.putnami.pwt.core.widget.client.PanelAccordion;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.plugin.code.client.StaticCode;
import fr.putnami.pwt.plugin.code.client.configuration.java.JavaConfiguration;
import fr.putnami.pwt.plugin.code.client.configuration.xml.XmlConfiguration;

public abstract class SamplePage extends Composite implements View {

	interface Binder extends UiBinderLocalized<Widget, SampleLayoutView> {
		UiBinderLocalized<Widget, SampleLayoutView> BINDER = GWT.create(Binder.class);
	}

	public static class SampleLayoutView {
		@UiField
		HTMLPanel sampleContent;
		@UiField
		PanelAccordion sourceAccordion;
		@UiField
		StaticCode sourceCode;
	}

	private class SourceItem extends ListItem implements ClickHandler {

		private final String fileName;

		public SourceItem(String fileName) {
			super();
			this.fileName = fileName;
			String simpleFileName = fileName;
			if (simpleFileName.lastIndexOf("/") != -1) {
				simpleFileName = simpleFileName.substring(simpleFileName.lastIndexOf("/") + 1);
			}
			Anchor<?> anchor = new Anchor<>(simpleFileName);
			anchor.addClickHandler(this);
			this.add(anchor);
		}

		@Override
		public void onClick(ClickEvent event) {
			SamplePage.this.requestFile(this.fileName);
		}
	}

	private final SampleLayoutView samplePageLayout = new SampleLayoutView();

	public SamplePage() {
		this.initWidget(Binder.BINDER.createAndBindUi(this.samplePageLayout));
		this.samplePageLayout.sourceCode.asWidget().setVisible(false);
	}

	protected void addSources(Multimap<String, String> sources) {
		Panel panelToOpen = null;
		String sourceToOpen = null;
		for (String panelName : sources.keySet()) {

			List sourceList = new List();
			sourceList.setType(Type.LIST);
			for (String source : sources.get(panelName)) {
				if (sourceToOpen == null) {
					sourceToOpen = source;
				}
				sourceList.add(new SourceItem(source));
			}
			Panel sourcePanel = new Panel();
			if (panelToOpen == null) {
				panelToOpen = sourcePanel;
			}
			sourcePanel.add(new Header(panelName));
			sourcePanel.add(sourceList);
			this.samplePageLayout.sourceAccordion.add(sourcePanel);
		}
		this.requestFile(sourceToOpen);
	}

	@PresentHandler
	public void present() {
		this.samplePageLayout.sampleContent.add(this.getSampleWidget());
	}

	private void requestFile(final String fileName) {
		this.samplePageLayout.sourceCode.asWidget().setVisible(false);
		this.samplePageLayout.sourceCode.setText("");

		RequestCallback callBack = new RequestCallback() {

			@Override
			public void onResponseReceived(Request request, Response response) {
				if (fileName.endsWith("xml")) {
					SamplePage.this.samplePageLayout.sourceCode
							.setConfiguration(XmlConfiguration.XML_CONFIGURATION);
				} else if (fileName.endsWith("java")) {
					SamplePage.this.samplePageLayout.sourceCode
							.setConfiguration(JavaConfiguration.JAVA_CONFIGURATION);
				} else {
					SamplePage.this.displayError(new RuntimeException("Unknow file type"));
				}
				SamplePage.this.samplePageLayout.sourceCode.setText(response.getText());
				SamplePage.this.samplePageLayout.sourceCode.asWidget().setVisible(true);
			}

			@Override
			public void onError(Request request, Throwable exception) {
				SamplePage.this.displayError(exception);
			}
		};
		RequestBuilder builder =
				new RequestBuilder(RequestBuilder.GET, GWT.getModuleBaseURL() + "sample/" + fileName);
		builder.setCallback(callBack);
		try {
			builder.send();
		} catch (RequestException e) {
			callBack.onError(null, e);
		}
	}

	private void displayError(Throwable exception) {
		GWT.reportUncaughtException(exception);
	}

	protected abstract IsWidget getSampleWidget();
}

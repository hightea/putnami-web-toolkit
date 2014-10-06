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
package fr.putnami.pwt.plugin.code.client;

import com.google.common.base.Objects;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasHTML;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.core.widget.client.assist.ContentAssistAspect;
import fr.putnami.pwt.core.widget.client.base.AbstractInput;
import fr.putnami.pwt.core.widget.client.event.ChangeEvent;
import fr.putnami.pwt.core.widget.client.helper.CompositeFocusHelper;
import fr.putnami.pwt.core.widget.client.util.HTMLUtils;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.plugin.code.client.aspect.CodeEditorAspect;
import fr.putnami.pwt.plugin.code.client.base.CodeEditor;
import fr.putnami.pwt.plugin.code.client.base.CodeEditorDriver;
import fr.putnami.pwt.plugin.code.client.base.CodeEditorDriverImpl;
import fr.putnami.pwt.plugin.code.client.configuration.CodeEditorConfiguration;
import fr.putnami.pwt.plugin.code.client.input.CodeInput;
import fr.putnami.pwt.plugin.code.client.input.CodeInputImpl;
import fr.putnami.pwt.plugin.code.client.output.CodeOutput;
import fr.putnami.pwt.plugin.code.client.output.CodeOutputImpl;

public class InputCode extends AbstractInput<String> implements CodeEditor, HasPlaceholder,
FocusHandler, ClickHandler, BlurHandler, HasHTML {

	private final FlowPanel content;

	private final CodeInput codeInput = new CodeInputImpl();
	private final CodeOutput codeOutput = new CodeOutputImpl();
	private final CodeEditorDriver codeDriver = new CodeEditorDriverImpl(this.codeInput,
			this.codeOutput);

	private HandlerRegistration valueChangeRegistration;

	private CompositeFocusHelper compositeFocus;

	private boolean displayInput = false;

	public InputCode() {
		super(new FlowPanel());
		this.content = (FlowPanel) this.getWidget();
		this.endConstruct();
	}

	protected InputCode(InputCode source) {
		super(new FlowPanel(), source);
		this.content = (FlowPanel) this.getWidget();
		this.endConstruct();
		if (source != null) {
			this.codeDriver.applyConfiguration(source.codeDriver);
		}
	}

	@Override
	protected void endConstruct() {
		this.content.add(this.codeOutput);
		// Hook to replace Input by output on blur
		this.setTabIndex(0);
		this.compositeFocus = CompositeFocusHelper.createFocusHelper(this.codeInput, this.codeInput);

		this.addFocusHandler(this);
		// in IE, the focus is not set on click on the codeOutput
		this.codeOutput.asWidget().addDomHandler(this, ClickEvent.getType());
		this.compositeFocus.addBlurHandler(this);

		StyleUtils.removeStyle(this, AbstractInput.STYLE_CONTROL);
		StyleUtils.addStyle(this.codeInput.asWidget(), AbstractInput.STYLE_CONTROL);

		super.endConstruct();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputCode(this);
	}

	@Override
	public String getPlaceholder() {
		return this.codeInput.getPlaceholder();
	}

	@Override
	public void setPlaceholder(String placeholder) {
		this.codeInput.setPlaceholder(placeholder);
	}

	@Override
	public boolean isDirty() {
		return !Objects.equal(this.getValue(), this.codeDriver.getValue());
	}

	@Override
	public HandlerRegistration addDirtyHandler(Handler handler) {
		if (this.valueChangeRegistration == null) {
			// Hook to prevent blur on click on suggestion popup
			this.valueChangeRegistration =
					this.codeInput.addValueChangeHandler(new ChangeEvent<String>(InputCode.this));
		}
		return super.addDirtyHandler(handler);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addValueChangeHandler(
			ValueChangeHandler<String> handler) {
		return this.codeInput.addValueChangeHandler(handler);
	}

	@Override
	public String flush() {
		String value = this.codeDriver.flush();
		this.validate(value);
		if (!this.hasErrors()) {
			this.setValue(value);
		}
		return this.getValue();
	}

	@Override
	public void edit(String value) {
		this.setValue(value);
		this.codeDriver.edit(value);
	}

	@Override
	public void applyConfiguration(CodeEditorConfiguration configuration) {
		this.codeDriver.applyConfiguration(configuration);
		for (CodeEditorAspect aspect : this.codeDriver.getAspects()) {
			if (aspect instanceof ContentAssistAspect) {
				this.compositeFocus.addFocusPartner(((ContentAssistAspect) aspect).getSuggestionWidget()
						.getElement());
			}
		}
	}

	@Override
	public void setConfiguration(CodeEditorConfiguration configuration) {
		this.codeDriver.setConfiguration(configuration);
	}

	@Override
	public void onClick(ClickEvent event) {
		this.showInput();
	}

	@Override
	public void onFocus(FocusEvent event) {
		this.showInput();
	}

	@Override
	public void onBlur(BlurEvent event) {
		this.hideInput();
	}

	private void showInput() {
		if (!this.displayInput) {
			this.displayInput = true;
			this.codeInput.asWidget().getElement().getStyle().setHeight(
					this.codeOutput.asWidget().getElement().getOffsetHeight(), Unit.PX);
			this.codeOutput.asWidget().removeFromParent();
			this.content.add(this.codeInput);
			this.codeInput.setFocus(true);
		}
	}

	private void hideInput() {
		if (this.displayInput) {
			this.displayInput = false;
			this.codeInput.asWidget().removeFromParent();
			this.content.add(this.codeOutput);
		}
	}

	@Override
	public String getText() {
		return this.getValue();
	}

	@Override
	public void setText(String text) {
		this.edit(text);
	}

	@Override
	public String getHTML() {
		return this.getText();
	}

	/**
	 * To be use only by UIBinder (provide escaped HTML)
	 */
	@Override
	public void setHTML(String html) {
		String htmlToEdit = HTMLUtils.unescapeHTML(html);
		this.setText(htmlToEdit);
	}
}

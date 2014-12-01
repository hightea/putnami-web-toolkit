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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.DivElement;
import com.google.gwt.editor.client.LeafValueEditor;
import com.google.gwt.user.client.ui.Focusable;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorError;
import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.EditorValue;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.editor.client.helper.TakesValueEditorWrapper;
import fr.putnami.pwt.core.model.client.base.EditorProvider;
import fr.putnami.pwt.core.model.client.base.HasEditorProvider;
import fr.putnami.pwt.core.model.client.base.HasInputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasLabelEditor;
import fr.putnami.pwt.core.model.client.base.HasOutputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractForm.Layout;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.HasFormType;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class FormGroup<T> extends AbstractPanel
	implements HasFormType, CloneableWidget, EditorValue<T>, HasLabelEditor, HasEditorProvider,
	HasOutputEditorFactory, HasInputEditorFactory, HasReadonly, EditorLabel, EditorError, Focusable {

	private static final CssStyle STYLE_FORM_GROUP = new SimpleStyle("form-group");
	private static final CssStyle STYLE_ERROR = new SimpleStyle("has-error");
	private static final CssStyle STYLE_SCREAN_READER = new SimpleStyle("sr-only");

	private Layout type;

	private EditorInput<T> inputFactory;
	private EditorOutput<T> outputFactory;

	private EditorProvider editorProvider;

	private EditorLabel label;
	private Help help;
	private ErrorGroup error;

	private Integer tabIndex;
	private Character accessKey;
	private Boolean readonly;
	private T value;

	private Editor editor;

	public FormGroup() {
		super(DivElement.TAG);
		StyleUtils.addStyle(this, FormGroup.STYLE_FORM_GROUP);
	}

	protected FormGroup(FormGroup<T> source) {
		super(source);
		this.type = source.type;
		this.inputFactory = WidgetUtils.cloneWidget(source.inputFactory);
		this.outputFactory = WidgetUtils.cloneWidget(source.outputFactory);
		this.label = WidgetUtils.cloneWidget(source.label);
		this.help = WidgetUtils.cloneWidget(source.help);
		this.error = WidgetUtils.cloneWidget(source.error);
		this.readonly = source.readonly;
		this.tabIndex = source.tabIndex;
		this.accessKey = source.accessKey;
	}

	@Override
	public IsWidget cloneWidget() {
		return new FormGroup<T>(this);
	}

	@Override
	public Boolean getReadonly() {
		return this.readonly;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}

	@Override
	public EditorInput<T> getInputFactory() {
		return this.inputFactory;
	}

	@Override
	public EditorOutput<T> getOutputFactory() {
		return this.outputFactory;
	}

	@Override
	public EditorLabel getLabelEditor() {
		return this.label;
	}

	@Override
	public void setLabelEditor(EditorLabel editor) {
		this.label = editor;
	}

	@Override
	public void setEditorProvider(EditorProvider provider) {
		this.editorProvider = provider;
	}

	@Override
	public Layout getLayout() {
		return this.type;
	}

	@Override
	public void setLayout(Layout type) {
		this.type = type;
	}

	@Override
	public void add(IsWidget child) {
		if (child instanceof EditorInput) {
			assert this.inputFactory == null : "inputFactory may only be set once";
			this.inputFactory = (EditorInput<T>) child;
		}
		if (child instanceof EditorOutput) {
			assert this.outputFactory == null : "outputFactory may only be set once";
			this.outputFactory = (EditorOutput) child;
		}

		if (child instanceof Label) {
			this.label = (Label) child;
			this.addEditor(child);
		} else if (child instanceof Help) {
			this.help = (Help) child;
		}
	}

	@Override
	public boolean isLabelMandatory() {
		return false;
	}

	@Override
	public String getLabelKey() {
		return null;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {EditorLabel.HELP_SUFFIX};
	}

	@Override
	public String getText() {
		if (this.help == null) {
			return null;
		}
		return this.help.getText();
	}

	@Override
	public void setText(String message) {
		if (this.help == null) {
			this.help = new Help();
		}
		this.help.setText(message);
	}

	@Override
	public void clearErrors() {
		if (this.error != null) {
			this.error.clearErrors();
			this.error = null;
			StyleUtils.removeStyle(this, FormGroup.STYLE_ERROR);
		}
	}

	@Override
	public void displayErrors(Iterable<Error> errors) {
		if (this.error == null) {
			this.error = new ErrorGroup();
		}
		this.error.displayErrors(errors);
		this.error.redraw();
		StyleUtils.toggleStyle(this, FormGroup.STYLE_ERROR, !Boolean.TRUE.equals(this.readonly) && this.error != null
			&& this.error.hasError());
	}

	@Override
	public void redraw() {
		this.clear();

		StyleUtils
			.toggleStyle(Widget.asWidgetOrNull(this.label), FormGroup.STYLE_SCREAN_READER, this.type == Layout.INLINE);
		this.addIfNotNull(this.label, 3, 0, false);
		boolean ro = isReadOnly();
		this.editor = this.editorProvider.getEditor(ro);
		initFocusableEditor();
		if (!Boolean.FALSE.equals(this.readonly)) {
			this.addIfNotNull(editor, 9, 0, true);
		} else {
			this.addIfNotNull(editor, 9, 0, true);
			this.addIfNotNull(this.error, 9, 3, true);
			this.addIfNotNull(this.help, 9, 3, true);
		}
	}

	@Override
	public T getValue() {
		return this.value;
	}

	@Override
	public void edit(T value) {
		this.value = value;
	}

	@Override
	public LeafValueEditor<T> asEditor() {
		return new TakesValueEditorWrapper<T>(this);
	}

	@Override
	public void setTabIndex(int tabIndex) {
		this.tabIndex = tabIndex;
	}

	@Override
	public int getTabIndex() {
		return tabIndex;
	}

	@Override
	public void setAccessKey(char accessKey) {
		this.accessKey = accessKey;
	}

	@Override
	public void setFocus(boolean focused) {
		if (editor instanceof Focusable) {
			Focusable focusable = (Focusable) editor;
			focusable.setFocus(focused);
		}
	}

	private void initFocusableEditor() {
		if (editor instanceof Focusable) {
			Focusable focusable = (Focusable) editor;
			if (accessKey != null) {
				focusable.setAccessKey(accessKey);
			}
			if (tabIndex != null) {
				focusable.setTabIndex(tabIndex);
			}
		}
	}

	private boolean isReadOnly() {
		return this.readonly == null ? false : this.readonly;
	}

	private void addIfNotNull(Editor e, int size, int offset, boolean wrap) {
		if (e instanceof IsWidget) {
			boolean wrapInCol = wrap;
			wrapInCol &= this.type == Layout.HORIZONTAL;

			Widget toAdd = Widget.asWidgetOrNull((IsWidget) e);
			if (wrapInCol) {
				GridColumn column = new GridColumn();
				column.add(toAdd);
				column.setSize(size);
				column.setOffset(offset);
				toAdd = column;
			}
			if (this.type == Layout.HORIZONTAL) {
				if (size > 0) {
					StyleUtils.addStyle(toAdd, new GridColumn.SizeStyle(GridColumn.PREFIX_SIZE_MD, size));
				}
				if (offset > 0) {
					StyleUtils.addStyle(toAdd, new GridColumn.OffsetStyle(GridColumn.PREFIX_OFFSET_MD, offset));
				}
			}
			this.append(toAdd);
		}
	}

}

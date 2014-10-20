package fr.putnami.pwt.core.editor.client.helper;

import com.google.gwt.editor.client.EditorDelegate;
import com.google.gwt.editor.client.HasEditorDelegate;
import com.google.gwt.editor.client.LeafValueEditor;

import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorValue;

public class TakesValueEditorWrapper<T> implements LeafValueEditor<T>, HasEditorDelegate<T> {

	private final EditorValue<T> editor;
	private EditorDelegate<T> delegate;

	public TakesValueEditorWrapper(EditorValue<T> editor) {
		this.editor = editor;
	}

	@Override
	public void setValue(T value) {
		editor.edit(value);
	}

	@Override
	public T getValue() {
		if (editor instanceof EditorInput) {
			EditorInput<T> input = (EditorInput<T>) editor;
			T result = input.flush();
			if (delegate != null && input.hasErrors()) {
				for (fr.putnami.pwt.core.editor.client.Error error : input.getErrors()) {
					delegate.recordError(error.getMessageKey(), error.getValue(), error.getParameters());
				}
			}
			return result;
		}
		return editor.getValue();
	}

	@Override
	public void setDelegate(EditorDelegate<T> delegate) {
		this.delegate = delegate;
	}

}
